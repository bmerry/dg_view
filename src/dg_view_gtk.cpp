/*
   This file is part of Datagrind, a tool for tracking data accesses.

   Copyright (C) 2010 Bruce Merry
      bmerry@users.sourceforge.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include <gtk/gtk.h>
#include <glib.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <sstream>

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include "dg_record.h"

using namespace std;

struct stack_trace_view
{
    GtkWidget *view;        /* GtkTreeView */
    GtkListStore *store;
};

enum stack_trace_column
{
    STC_ADDR = 0,
    STC_FUNCTION,
    STC_FILE,
    STC_LINE,
    STC_DSO
};

struct viewer;

struct viewer_region
{
    viewer *owner;

    GtkWidget *image;   /* GtkImage */
    GtkWidget *events;  /* GtkEventBox */
    GdkPixbuf *pixbuf;

    double addr_min, addr_max;
    double iseq_min, iseq_max;

    int click_x;
    int click_y;
    bool in_click;
};

struct viewer
{
    GtkWidget *window; /* GtkWindow */
    viewer_region region;

    GtkWidget *addr_entry; /* GtkEntry - displays access address */
    stack_trace_view access_stack;  /* Stack trace where the access occurred */
    stack_trace_view block_stack;  /* Stack trace where the memory was allocated */
};

static gboolean on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_resize(GtkWidget *widget, GtkAllocation *event, gpointer user_data);

static void build_region(viewer *v, viewer_region *vr, int width, int height)
{
    vr->owner = v;
    vr->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    if (vr->pixbuf == NULL)
    {
        fprintf(stderr, "Could not allocate a %d x %d pixbuf\n", width, height);
        exit(1);
    }

    vr->image = gtk_image_new_from_pixbuf(vr->pixbuf);
    if (vr->image == NULL)
    {
        fprintf(stderr, "Could not allocate an image for the pixbuf\n");
        exit(1);
    }
    g_object_unref(vr->pixbuf); /* vr->image will hold a ref for us */

    vr->events = gtk_event_box_new();
    if (vr->events == NULL)
    {
        fprintf(stderr, "Could not allocate an event box\n");
        exit(1);
    }
    gtk_widget_add_events(vr->events, GDK_BUTTON_PRESS_MASK);
    g_signal_connect(G_OBJECT(vr->events), "button-press-event",
                     G_CALLBACK(on_press), vr);
    g_signal_connect(G_OBJECT(vr->events), "button-release-event",
                     G_CALLBACK(on_release), vr);
    g_signal_connect(G_OBJECT(vr->events), "size-allocate",
                     G_CALLBACK(on_resize), vr);
    gtk_container_add(GTK_CONTAINER(vr->events), vr->image);

    gtk_widget_set_size_request(vr->image, width, height);

    gdk_pixbuf_fill(vr->pixbuf, 0x00000000);
}

/* Display an address in hex */
static void filter_stack_trace(GtkTreeModel *model,
                               GtkTreeIter *iter,
                               GValue *value,
                               gint column,
                               gpointer data)
{
    GtkTreeModel *child_model;
    GtkTreeIter child_iter;

    child_model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(model));
    gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(model), &child_iter, iter);
    switch (column)
    {
    case STC_ADDR:
        {
            ostringstream o;
            guint64 a;
            gtk_tree_model_get(child_model, &child_iter, STC_ADDR, &a, -1);
            o << showbase << hex << a;
            g_value_set_string(value, o.str().c_str());
        }
        break;
    case STC_LINE:
        {
            gint l;
            gtk_tree_model_get(child_model, &child_iter, STC_LINE, &l, -1);
            if (l == 0)
                g_value_set_string(value, "");
            else
            {
                ostringstream o;
                o << l;
                g_value_set_string(value, o.str().c_str());
            }
        }
        break;
    case STC_FILE:
    case STC_DSO:
        {
            /* Display only the basename of filenames */
            gchar *name;
            gchar *suffix;

            gtk_tree_model_get(child_model, &child_iter, column, &name, -1);
            suffix = g_strrstr(name, G_DIR_SEPARATOR_S);
            if (suffix == NULL)
                suffix = name;
            else
                suffix++; /* Skip last '/' */
            g_value_set_string(value, suffix);
            g_free(name);
        }
        break;
    default:
        g_value_unset(value);
        gtk_tree_model_get_value(child_model, &child_iter, column, value);
        break;
    }
}

/* Builds a tree view for a stack trace */
static void build_stack_trace_view(stack_trace_view *stv)
{
    GtkWidget *view;
    GtkListStore *store;
    GtkCellRenderer *cell;
    GtkTreeViewColumn *column;
    GtkTreeModel *filter;
    GtkWidget *scroll;

    const gint ncolumns = 5;
    GType store_types[ncolumns] =
    {
        G_TYPE_UINT64,  /* Address */
        G_TYPE_STRING,  /* Function */
        G_TYPE_STRING,  /* Filename */
        G_TYPE_INT,     /* Line number */
        G_TYPE_STRING   /* DSO */
    };
    GType filter_types[ncolumns] =
    {
        G_TYPE_STRING,
        G_TYPE_STRING,
        G_TYPE_STRING,
        G_TYPE_STRING,
        G_TYPE_STRING
    };

    store = gtk_list_store_newv(ncolumns, store_types);

    filter = gtk_tree_model_filter_new(GTK_TREE_MODEL(store), NULL);
    gtk_tree_model_filter_set_modify_func(GTK_TREE_MODEL_FILTER(filter),
                                          ncolumns, filter_types,
                                          filter_stack_trace, NULL, NULL);

    view = gtk_tree_view_new_with_model(filter);
    g_object_unref(filter); /* View holds a ref */

    cell = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("Address", cell,
                                                      "text", STC_ADDR,
                                                      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    cell = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("Function", cell,
                                                      "text", STC_FUNCTION,
                                                      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    cell = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("File", cell,
                                                      "text", STC_FILE,
                                                      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    cell = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("Line", cell,
                                                      "text", STC_LINE,
                                                      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    cell = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("DSO", cell,
                                                      "text", STC_DSO,
                                                      NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scroll, 400, 200);
    gtk_container_add(GTK_CONTAINER(scroll), view);

    stv->view = scroll;
    stv->store = store;
}

static void build_main_window(viewer *v)
{
    GtkWidget *table;
    GtkWidget *top_box;
    GtkWidget *info_box;
    GtkWidget *frame;
    GtkWidget *frame_box;
    GtkWidget *label;

    v->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(v->window), "dg_view");
    g_signal_connect(G_OBJECT(v->window), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);

    build_region(v, &v->region, 600, 600);
    build_stack_trace_view(&v->access_stack);
    build_stack_trace_view(&v->block_stack);

    info_box = gtk_vbox_new(FALSE, 0);

    /*** Access frame ***/

    frame = gtk_frame_new("Access");
    frame_box = gtk_vbox_new(FALSE, 0);

    label = gtk_label_new("Address");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    v->addr_entry = gtk_entry_new();
    gtk_entry_set_editable(GTK_ENTRY(v->addr_entry), FALSE);
    gtk_entry_set_width_chars(GTK_ENTRY(v->addr_entry), 19);
    gtk_box_pack_start(GTK_BOX(frame_box), v->addr_entry, FALSE, FALSE, 0);

    label = gtk_label_new("Stack trace");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(frame_box), v->access_stack.view, TRUE, TRUE, 0);

    gtk_container_add(GTK_CONTAINER(frame), frame_box);
    gtk_box_pack_start(GTK_BOX(info_box), frame, TRUE, TRUE, 0);

    /*** Memory block frame */

    frame = gtk_frame_new("Memory block");
    frame_box = gtk_vbox_new(FALSE, 0);

    label = gtk_label_new("Stack trace");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(frame_box), v->block_stack.view, TRUE, TRUE, 0);

    gtk_container_add(GTK_CONTAINER(frame), frame_box);
    gtk_box_pack_start(GTK_BOX(info_box), frame, TRUE, TRUE, 0);

    /*** Main region ***/

    table = gtk_table_new(1, 1, FALSE);
    gtk_table_attach_defaults(GTK_TABLE(table), v->region.events, 0, 1, 0, 1);

    /*** Stitch it all together ***/

    top_box = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_box), table, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(top_box), info_box, FALSE, FALSE, 0);

    gtk_container_add(GTK_CONTAINER(v->window), top_box);
    gtk_widget_show_all(v->window);
}

static void prepare_min_max(viewer_region *vr)
{
    const bbrun_list &bbruns = dg_view_bbruns();
    const page_map &pages = dg_view_page_map();
    vr->addr_min = pages.begin()->second;
    vr->addr_max = (--pages.end())->second + DG_VIEW_PAGE_SIZE;
    vr->iseq_min = bbruns.begin()->iseq_start;

    bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = dg_view_bbrun_get_bbdef(*last_bbrun);
    vr->iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;
}

/* Takes a range in a semantic space and gives first and last pixels covered.
 * The return values are a [first, last) range. Returns false if there is
 * no intersection with the viewport.
 */
static bool range_to_pixels(double lo, double hi,
                            double view_min, double view_max, int pixels,
                            int &pixel_lo, int &pixel_hi)
{
    if (hi <= view_min || lo >= view_max)
        return false;
    double scale = pixels / (view_max - view_min);
    pixel_lo = (int) floor((lo - view_min) * scale + 0.5);
    pixel_hi = (int) floor((hi - view_min) * scale + 0.f);
    if (pixel_hi == pixel_lo)
        pixel_hi++;
    pixel_lo = max(pixel_lo, 0);
    pixel_hi = min(pixel_hi, pixels);
    return true;
}

static void update_region(viewer_region *vr)
{
    int rowstride = gdk_pixbuf_get_rowstride(vr->pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(vr->pixbuf);
    int width = gdk_pixbuf_get_width(vr->pixbuf);
    int height = gdk_pixbuf_get_height(vr->pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(vr->pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(vr->pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(vr->pixbuf) == 8);
    gdk_pixbuf_fill(vr->pixbuf, 0);

    const bbrun_list &bbruns = dg_view_bbruns();
    for (bbrun_list::const_iterator bbr = bbruns.begin(); bbr != bbruns.end(); ++bbr)
    {
        for (size_t j = 0; j < bbr->n_addrs; j++)
            if (bbr->addrs[j])
            {
                const bbdef &bbd = dg_view_bbrun_get_bbdef(*bbr);
                g_assert(j < bbd.accesses.size());
                const bbdef_access &bbda = bbd.accesses[j];

                size_t addr = dg_view_remap_address(bbr->addrs[j]);
                size_t iseq = bbr->iseq_start + bbda.iseq;
                int x1, x2, y1, y2;

                if (range_to_pixels(addr, addr + bbda.size,
                                    vr->addr_min, vr->addr_max, width,
                                    x1, x2) &&
                    range_to_pixels(iseq, iseq + 1,
                                    vr->iseq_min, vr->iseq_max, height,
                                    y1, y2))
                {
                    for (int y = y1; y < y2; y++)
                        for (int x = x1; x < x2; x++)
                        {
                            guchar * const p = pixels + y * rowstride + x * n_channels;
                            switch (bbda.dir)
                            {
                            case DG_ACC_READ:
                                p[1] = 255; /* green */
                                break;
                            case DG_ACC_WRITE:
                                p[0] = 255; /* red */
                                break;
                            default:
                                g_assert_not_reached();
                            }
                        }
                }
            }
    }

    gtk_widget_queue_draw(vr->image);
}

static gboolean on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;

    vr->in_click = true;
    vr->click_x = event->x;
    vr->click_y = event->y;
    gtk_grab_add(widget);
    return FALSE;
}

/* Computes new values for amin and amax (which are in an abstract space,
 * based on two coordinates of a zoom box in a window space [0, size).
 */
static void update_zoom(double *amin, double *amax, int w1, int w2, int size)
{
    /* Convert to pixel centers */
    double l = min(w1, w2) + 0.5;
    double h = max(w1, w2) + 0.5;

    /* Convert to parametric coordinates in [0, 1] */
    l /= size;
    h /= size;

    double scale = h - l;
    /* Expand slightly so that some context is visible */
    double expand = 0.1f * scale;
    l -= expand;
    h += expand;

    /* Clamp to the window */
    l = max(l, 0.0);
    h = min(h, 1.0);

    /* Interpolate to get new coordinates */
    double old_size = *amax - *amin;
    *amax = *amin + h * old_size;
    *amin = *amin + l * old_size;
}

static void stack_trace_view_populate(stack_trace_view *stv, const vector<HWord> &st)
{
    gtk_list_store_clear(stv->store);
    for (size_t i = 0; i < st.size();i++)
    {
        GtkTreeIter iter;
        gtk_list_store_append(stv->store, &iter);
        guint64 addr = st[i];
        string function, file, dso;
        int line;
        dg_view_addr2info(st[i], function, file, line, dso);
        gtk_list_store_set(stv->store, &iter,
                           STC_ADDR, addr,
                           STC_FUNCTION, function.c_str(),
                           STC_FILE, file.c_str(),
                           STC_LINE, (gint) line,
                           STC_DSO, dso.c_str(),
                           -1);
    }
}

static gboolean on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    gtk_grab_remove(widget);

    viewer_region *vr = (viewer_region *) user_data;
    if (vr->in_click)
    {
        vr->in_click = false;
        int width = gdk_pixbuf_get_width(vr->pixbuf);
        int height = gdk_pixbuf_get_height(vr->pixbuf);
        if (abs(event->x - vr->click_x) > 2 && abs(event->y - vr->click_y) > 2)
        {
            /* Assume it was a drag to zoom */
            update_zoom(&vr->addr_min, &vr->addr_max,
                        vr->click_x, event->x, width);
            update_zoom(&vr->iseq_min, &vr->iseq_max,
                        vr->click_y, event->y, height);
            update_region(vr);
        }
        else
        {
            /* A click to get information */
            double addr_size = vr->addr_max - vr->addr_min;
            double iseq_size = vr->iseq_max - vr->iseq_min;

            double addr_scale = addr_size / width;
            double iseq_scale = iseq_size / height;
            double ratio = iseq_scale / addr_scale;

            HWord remapped = (HWord) (0.5 + (event->x + 0.5) * addr_scale + vr->addr_min);
            HWord addr = dg_view_revmap_addr(remapped);
            double iseq = (event->y + 0.5) * iseq_scale + vr->iseq_min;

            mem_access access = dg_view_nearest_access(addr, iseq, ratio);
            if (access.size != 0)
            {
                ostringstream addr_str;
                addr_str << showbase << hex << access.addr;
                gtk_entry_set_text(GTK_ENTRY(vr->owner->addr_entry), addr_str.str().c_str());

                mem_block *block = access.block;
                if (block != NULL)
                {
                    stack_trace_view_populate(&vr->owner->block_stack, block->stack);
                }
                else
                {
                    /* Clear it */
                    stack_trace_view_populate(&vr->owner->block_stack, vector<HWord>());
                }

                stack_trace_view_populate(&vr->owner->access_stack, access.stack);
            }
        }
    }
    return FALSE;
}

static gboolean on_resize(GtkWidget *widget, GtkAllocation *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;

    if (event->width != gdk_pixbuf_get_width(vr->pixbuf)
        || event->height != gdk_pixbuf_get_height(vr->pixbuf))
    {
        GdkPixbuf *new_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8,
                                               event->width, event->height);
        g_return_val_if_fail(new_pixbuf != NULL, FALSE);
        gtk_image_set_from_pixbuf(GTK_IMAGE(vr->image), new_pixbuf);
        vr->pixbuf = new_pixbuf;
        g_object_unref(new_pixbuf); /* The GtkImage holds a ref for us */
        update_region(vr);
    }
    return FALSE;
}

int main(int argc, char **argv)
{
    viewer main_viewer;
    gtk_init(&argc, &argv);

    dg_view_parse_opts(&argc, argv);
    if (argc != 2)
    {
        dg_view_usage(argv[0], 2);
    }
    if (!dg_view_load(argv[1]))
        return 1;

    build_main_window(&main_viewer);
    prepare_min_max(&main_viewer.region);
    update_region(&main_viewer.region);
    gtk_main();
    return 0;
}
