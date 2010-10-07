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

struct dimension
{
    double cached_min;
    double cached_max;
    GtkAdjustment *adj;
};

struct viewer_region
{
    viewer *owner;

    GtkWidget *image;   /* GtkImage */
    GtkWidget *events;  /* GtkEventBox */
    GtkWidget *top;     /* Top-level widget to add to parents */
    GdkPixbuf *pixbuf;

    dimension dims[2];

    int click_x;
    int click_y;
    bool in_click;
};

struct viewer
{
    GtkWidget *window;             /* GtkWindow */
    viewer_region region;

    GtkAdjustment *addr_adj;
    GtkAdjustment *iseq_adj;

    GtkWidget *addr_entry;         /* GtkEntry - displays access address */
    GtkWidget *block_addr_entry;   /* GtkEntry - displays base of memory block */
    GtkWidget *block_size_entry;   /* GtkEntry - displays size of memory block */
    GtkWidget *block_offset_entry; /* GtkEntry - displays offset of address in block */

    stack_trace_view access_stack; /* Stack trace where the access occurred */
    stack_trace_view block_stack;  /* Stack trace where the memory was allocated */
};

static gboolean on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_resize(GtkWidget *widget, GtkAllocation *event, gpointer user_data);
static void on_view_changed(GtkAdjustment *adj, gpointer user_data);
static void on_zoom_out_addr(GtkToolButton *button, gpointer user_data);
static void on_zoom_in_addr(GtkToolButton *button, gpointer user_data);
static void on_zoom_out_iseq(GtkToolButton *button, gpointer user_data);
static void on_zoom_in_iseq(GtkToolButton *button, gpointer user_data);

static inline double viewer_region_min(const viewer_region *vr, int d)
{
    return gtk_adjustment_get_value(vr->dims[d].adj);
}

static inline double viewer_region_range(const viewer_region *vr, int d)
{
    return gtk_adjustment_get_page_size(vr->dims[d].adj);
}

static inline double viewer_region_max(const viewer_region *vr, int d)
{
    return viewer_region_min(vr, d) + viewer_region_range(vr, d);
}

static void build_region(viewer *v, viewer_region *vr,
                         int width, int height,
                         GtkAdjustment *addr_adj, GtkAdjustment *iseq_adj)
{
    vr->owner = v;
    vr->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    if (vr->pixbuf == NULL)
    {
        fprintf(stderr, "Could not allocate a %d x %d pixbuf\n", width, height);
        exit(1);
    }
    gdk_pixbuf_fill(vr->pixbuf, 0x00000000);

    vr->image = gtk_image_new_from_pixbuf(vr->pixbuf);
    if (vr->image == NULL)
    {
        fprintf(stderr, "Could not allocate an image for the pixbuf\n");
        exit(1);
    }
    g_object_unref(vr->pixbuf); /* vr->image will hold a ref for us */
    gtk_widget_set_size_request(vr->image, width, height);

    vr->events = gtk_event_box_new();
    if (vr->events == NULL)
    {
        fprintf(stderr, "Could not allocate an event box\n");
        exit(1);
    }
    gtk_widget_add_events(vr->events, GDK_BUTTON_PRESS_MASK);
    gtk_container_add(GTK_CONTAINER(vr->events), vr->image);

    g_signal_connect(G_OBJECT(vr->events), "button-press-event",
                     G_CALLBACK(on_press), vr);
    g_signal_connect(G_OBJECT(vr->events), "button-release-event",
                     G_CALLBACK(on_release), vr);
    g_signal_connect(G_OBJECT(vr->events), "size-allocate",
                     G_CALLBACK(on_resize), vr);
    vr->dims[0].adj = addr_adj;
    vr->dims[1].adj = iseq_adj;
    for (int i = 0; i < 2; i++)
    {
        vr->dims[i].cached_min = -1.0;  // mark cache invalid
        vr->dims[i].cached_max = -1.0;
        g_signal_connect(G_OBJECT(vr->dims[i].adj), "changed",
                         G_CALLBACK(on_view_changed), vr);
        g_signal_connect(G_OBJECT(vr->dims[i].adj), "value-changed",
                         G_CALLBACK(on_view_changed), vr);
    }


    GtkWidget *hscroll = gtk_hscrollbar_new(vr->dims[0].adj);
    GtkWidget *vscroll = gtk_vscrollbar_new(vr->dims[1].adj);
    vr->top = gtk_table_new(2, 2, FALSE);
    gtk_table_attach(GTK_TABLE(vr->top), vr->events, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     0, 0);
    gtk_table_attach(GTK_TABLE(vr->top), hscroll, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) 0,
                     0, 0);
    gtk_table_attach(GTK_TABLE(vr->top), vscroll, 1, 2, 0, 1,
                     (GtkAttachOptions) 0,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     0, 0);
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
        {
            gchar *name;
            gtk_tree_model_get(child_model, &child_iter, column, &name, -1);
            string abbrev = dg_view_abbrev_file(name);
            g_free(name);
            g_value_set_string(value, abbrev.c_str());
        }
        break;
    case STC_DSO:
        {
            gchar *name;
            gtk_tree_model_get(child_model, &child_iter, column, &name, -1);
            string abbrev = dg_view_abbrev_dso(name);
            g_free(name);
            g_value_set_string(value, abbrev.c_str());
        }
        break;
    default:
        g_value_unset(value);
        gtk_tree_model_get_value(child_model, &child_iter, column, value);
        break;
    }
}

static void stack_trace_row_activated(GtkTreeView *view,
                                      GtkTreePath *path,
                                      GtkTreeViewColumn *column,
                                      gpointer user_data)
{
    stack_trace_view *stv = (stack_trace_view *) user_data;
    GtkTreeIter iter;
    gchar *file;
    gint line;

    gtk_tree_model_get_iter(GTK_TREE_MODEL(stv->store), &iter, path);
    gtk_tree_model_get(GTK_TREE_MODEL(stv->store), &iter,
                       STC_FILE, &file,
                       STC_LINE, &line,
                       -1);

    if (file != NULL && file[0] != '\0' && line > 0)
    {
        GPid pid;
        const gchar *argv[20];
        ostringstream line_arg_s;
        string line_arg;
        int argc = 0;

        line_arg_s << "+" << line << '\n';
        line_arg = line_arg_s.str();

        argv[argc++] = "gvim";
        argv[argc++] = "-f";
        argv[argc++] = "--servername";
        argv[argc++] = "dg_view";
        argv[argc++] = "--remote-silent";
        argv[argc++] = line_arg.c_str();
        argv[argc++] = file;
        argv[argc] = NULL;
        if (g_spawn_async(NULL,                     /* working_directory */
                          (gchar **) argv,
                          NULL,                     /* envp */
                          G_SPAWN_SEARCH_PATH,
                          NULL,                     /* child_setup */
                          NULL,                     /* user_data */
                          &pid,
                          NULL))                    /* error */
        {
            g_spawn_close_pid(pid);
        }
    }

    g_free(file);
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

    g_signal_connect(G_OBJECT(view), "row-activated",
                     G_CALLBACK(stack_trace_row_activated), stv);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scroll, 400, 200);
    gtk_container_add(GTK_CONTAINER(scroll), view);

    stv->view = scroll;
    stv->store = store;
}

static void prepare_min_max(viewer *v)
{
    const bbrun_list &bbruns = dg_view_bbruns();
    const page_map &pages = dg_view_page_map();

    double addr_min = pages.begin()->second;
    double addr_max = (--pages.end())->second + DG_VIEW_PAGE_SIZE;
    double iseq_min = bbruns.begin()->iseq_start;

    bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = dg_view_bbrun_get_bbdef(*last_bbrun);
    double iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;

    v->addr_adj = GTK_ADJUSTMENT(gtk_adjustment_new(addr_min, addr_min, addr_max,
                                                    addr_max - addr_min,
                                                    0.1 * (addr_max - addr_min),
                                                    addr_max - addr_min));
    v->iseq_adj = GTK_ADJUSTMENT(gtk_adjustment_new(iseq_min, iseq_min, iseq_max,
                                                    iseq_max - iseq_min,
                                                    0.1 * (iseq_max - iseq_min),
                                                    iseq_max - iseq_min));
}

static GtkWidget *build_toolbar(viewer *v)
{
    GtkWidget *toolbar;
    GtkToolItem *button;

    toolbar = gtk_toolbar_new();

    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_IN);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(on_zoom_in_addr), v);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);
    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_OUT);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(on_zoom_out_addr), v);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);

    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), gtk_separator_tool_item_new(), -1);

    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_IN);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(on_zoom_in_iseq), v);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);
    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_OUT);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(on_zoom_out_iseq), v);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);

    return toolbar;
}

static GtkWidget *build_access_frame(viewer *v)
{
    GtkWidget *frame, *frame_box, *label;

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
    return frame;
}

static GtkWidget *build_block_frame(viewer *v)
{
    GtkWidget *frame, *frame_box, *label;

    frame = gtk_frame_new("Memory block");
    frame_box = gtk_vbox_new(FALSE, 0);

    label = gtk_label_new("Address");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    v->block_addr_entry = gtk_entry_new();
    gtk_entry_set_editable(GTK_ENTRY(v->block_addr_entry), FALSE);
    gtk_entry_set_width_chars(GTK_ENTRY(v->block_addr_entry), 19);
    gtk_box_pack_start(GTK_BOX(frame_box), v->block_addr_entry, FALSE, FALSE, 0);

    label = gtk_label_new("Size");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    v->block_size_entry = gtk_entry_new();
    gtk_entry_set_editable(GTK_ENTRY(v->block_size_entry), FALSE);
    gtk_entry_set_width_chars(GTK_ENTRY(v->block_size_entry), 10);
    gtk_box_pack_start(GTK_BOX(frame_box), v->block_size_entry, FALSE, FALSE, 0);

    label = gtk_label_new("Offset");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    v->block_offset_entry = gtk_entry_new();
    gtk_entry_set_editable(GTK_ENTRY(v->block_offset_entry), FALSE);
    gtk_entry_set_width_chars(GTK_ENTRY(v->block_offset_entry), 10);
    gtk_box_pack_start(GTK_BOX(frame_box), v->block_offset_entry, FALSE, FALSE, 0);

    label = gtk_label_new("Stack trace");
    gtk_misc_set_alignment(GTK_MISC(label), 0.0f, 0.0f);
    gtk_box_pack_start(GTK_BOX(frame_box), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(frame_box), v->block_stack.view, TRUE, TRUE, 0);

    gtk_container_add(GTK_CONTAINER(frame), frame_box);
    return frame;
}

static void build_main_window(viewer *v)
{
    GtkWidget *table, *body_box, *info_box, *all_box, *toolbar, *frame;

    prepare_min_max(v);

    /*** Window ***/

    v->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(v->window), "dg_view");
    g_signal_connect(G_OBJECT(v->window), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);

    build_region(v, &v->region, 600, 600, v->addr_adj, v->iseq_adj);
    build_stack_trace_view(&v->access_stack);
    build_stack_trace_view(&v->block_stack);

    all_box = gtk_vbox_new(FALSE, 0);
    body_box = gtk_hbox_new(FALSE, 0);
    info_box = gtk_vbox_new(FALSE, 0);

    /*** Access frame ***/
    frame = build_access_frame(v);
    gtk_box_pack_start(GTK_BOX(info_box), frame, TRUE, TRUE, 0);

    /*** Memory block frame */
    frame = build_block_frame(v);
    gtk_box_pack_start(GTK_BOX(info_box), frame, TRUE, TRUE, 0);

    /*** Main region ***/
    table = gtk_table_new(1, 1, FALSE);
    gtk_table_attach_defaults(GTK_TABLE(table), v->region.top, 0, 1, 0, 1);

    /*** Toolbar ***/
    toolbar = build_toolbar(v);

    /*** Stitch it all together ***/

    gtk_box_pack_start(GTK_BOX(body_box), table, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(body_box), info_box, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(all_box), toolbar, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(all_box), body_box, TRUE, TRUE, 0);

    gtk_container_add(GTK_CONTAINER(v->window), all_box);
    gtk_widget_show_all(v->window);
}

/* Takes an address in a semantic space and gives the nearest pixel.
 * Returns false if it falls outside the viewport.
 */
static bool location_to_pixel(double a, double view_min, double view_max,
                              int pixels, int &out)
{
    if (a <= view_min || a >= view_max)
        return false;
    double scale = pixels / (view_max - view_min);
    out = (int) floor((a - view_min) * scale + 0.5);
    out = max(min(out, pixels - 1), 0);
    return true;
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
    pixel_hi = (int) floor((hi - view_min) * scale + 0.5);
    if (pixel_hi == pixel_lo)
        pixel_hi++;
    pixel_lo = max(pixel_lo, 0);
    pixel_hi = min(pixel_hi, pixels);
    return true;
}

static void update_region_vlines(viewer_region *vr)
{
    int rowstride = gdk_pixbuf_get_rowstride(vr->pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(vr->pixbuf);
    int width = gdk_pixbuf_get_width(vr->pixbuf);
    int height = gdk_pixbuf_get_height(vr->pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(vr->pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(vr->pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(vr->pixbuf) == 8);

    const double addr_min = viewer_region_min(vr, 0);
    const double addr_max = viewer_region_max(vr, 0);
    const double xrate = viewer_region_range(vr, 0) / width;
    const page_map &pm = dg_view_page_map();
    const uint8_t color_cut[3] = {192, 192, 192};
    const uint8_t color_page[3] = {64, 64, 64};
    const uint8_t color_cache[3] = {96, 32, 32};
    int x;
    HWord last = 0;
    for (page_map::const_iterator i = pm.begin(); i != pm.end(); ++i)
    {
        if (i->first != last + DG_VIEW_PAGE_SIZE)
        {
            if (location_to_pixel(i->second, addr_min, addr_max, width, x))
            {
                unsigned int ofs = n_channels * x;
                for (int j = 0; j < height; j++, ofs += rowstride)
                    for (int k = 0; k < 3; k++)
                        pixels[ofs + k] = color_cut[k];
            }
        }
        else if (xrate < DG_VIEW_PAGE_SIZE / 8)
        {
            if (location_to_pixel(i->second, addr_min, addr_max, width, x))
            {
                unsigned int ofs = n_channels * x;
                for (int j = 0; j < height; j++, ofs += rowstride)
                    for (int k = 0; k < 3; k++)
                        pixels[ofs + k] = color_page[k];
            }
        }
        if (xrate < DG_VIEW_LINE_SIZE / 8)
        {
            for (int c = DG_VIEW_LINE_SIZE; c < DG_VIEW_PAGE_SIZE; c += DG_VIEW_LINE_SIZE)
            {
                if (location_to_pixel(i->second + c, addr_min, addr_max, width, x))
                {
                    unsigned int ofs = n_channels * x;
                    for (int j = 0; j < height; j++, ofs += rowstride)
                        for (int k = 0; k < 3; k++)
                            pixels[ofs + k] = color_cache[k];
                }
            }
        }
        last = i->first;
    }
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

    update_region_vlines(vr);

    const bbrun_list &bbruns = dg_view_bbruns();
    const double addr_min = viewer_region_min(vr, 0);
    const double addr_max = viewer_region_max(vr, 0);
    const double iseq_min = viewer_region_min(vr, 1);
    const double iseq_max = viewer_region_max(vr, 1);
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
                                    addr_min, addr_max, width,
                                    x1, x2) &&
                    range_to_pixels(iseq, iseq + 1,
                                    iseq_min, iseq_max, height,
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

    for (int i = 0; i < 2; i++)
    {
        vr->dims[i].cached_min = gtk_adjustment_get_value(vr->dims[i].adj);
        vr->dims[i].cached_max = vr->dims[i].cached_min + gtk_adjustment_get_page_size(vr->dims[i].adj);
    }
}

static void on_view_changed(GtkAdjustment *adj, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;
    bool dirty = false;

    for (int i = 0; i < 2; i++)
    {
        double min = gtk_adjustment_get_value(vr->dims[i].adj);
        double max = min + gtk_adjustment_get_page_size(vr->dims[i].adj);
        if (vr->dims[i].cached_min != min || vr->dims[i].cached_max != max)
            dirty = true;
    }
    if (dirty)
        update_region(vr);
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

/* Computes new values for dimension, based on two coordinates of a zoom box in
 * a window space [0, size).
 *
 * If massage is true, the zoom area is expanded slightly then clamped. It
 * should be true when doing click-and-drag zooming, and false when zooming
 * in response to toolbar buttons.
 */
static void update_zoom(dimension *dim, int w1, int w2, int size, bool massage)
{
    /* Convert to pixel centers */
    double l = min(w1, w2) + 0.5;
    double h = max(w1, w2) + 0.5;

    /* Convert to parametric coordinates in [0, 1] */
    l /= size;
    h /= size;

    if (massage)
    {
        double scale = h - l;
        /* Expand slightly so that some context is visible */
        double expand = 0.1f * scale;
        l -= expand;
        h += expand;

        /* Clamp to the window */
        l = max(l, 0.0);
        h = min(h, 1.0);
    }

    /* Interpolate to get new coordinates */
    double old_size = gtk_adjustment_get_page_size(dim->adj);
    double value = gtk_adjustment_get_value(dim->adj) + l * old_size;
    double end = value + old_size * (h - l);

    /* Clamp to maximum range of adjustment */
    value = max(value, gtk_adjustment_get_lower(dim->adj));
    end = min(end, gtk_adjustment_get_upper(dim->adj));

    double page_size = end - value;
    gtk_adjustment_set_page_size(dim->adj, page_size);
    gtk_adjustment_set_page_increment(dim->adj, page_size);
    gtk_adjustment_set_step_increment(dim->adj, 0.1 * page_size);
    gtk_adjustment_set_value(dim->adj, value);
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
            g_object_freeze_notify(G_OBJECT(vr->dims[0].adj));
            g_object_freeze_notify(G_OBJECT(vr->dims[1].adj));
            update_zoom(&vr->dims[0], vr->click_x, event->x, width, true);
            update_zoom(&vr->dims[1], vr->click_y, event->y, height, true);
            g_object_thaw_notify(G_OBJECT(vr->dims[1].adj));
            g_object_thaw_notify(G_OBJECT(vr->dims[0].adj));
        }
        else
        {
            /* A click to get information */
            const double addr_min = viewer_region_min(vr, 0);
            const double addr_size = viewer_region_range(vr, 0);
            const double iseq_min = viewer_region_min(vr, 1);
            const double iseq_size = viewer_region_range(vr, 1);

            double addr_scale = addr_size / width;
            double iseq_scale = iseq_size / height;
            double ratio = iseq_scale / addr_scale;

            HWord remapped = (HWord) (0.5 + (event->x + 0.5) * addr_scale + addr_min);
            HWord addr = dg_view_revmap_addr(remapped);
            double iseq = (event->y + 0.5) * iseq_scale + iseq_min;

            mem_access access = dg_view_nearest_access(addr, iseq, ratio);
            if (access.size != 0)
            {
                ostringstream addr_str;
                addr_str << showbase << hex << access.addr;
                gtk_entry_set_text(GTK_ENTRY(vr->owner->addr_entry), addr_str.str().c_str());

                mem_block *block = access.block;
                if (block != NULL)
                {
                    addr_str.str("");
                    addr_str << block->addr;
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_addr_entry), addr_str.str().c_str());

                    ostringstream size_str;
                    size_str << block->size;
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_size_entry), size_str.str().c_str());

                    ostringstream offset_str;
                    offset_str << access.addr - block->addr;
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_offset_entry), offset_str.str().c_str());

                    stack_trace_view_populate(&vr->owner->block_stack, block->stack);
                }
                else
                {
                    /* Clear it */
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_addr_entry), "");
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_size_entry), "");
                    gtk_entry_set_text(GTK_ENTRY(vr->owner->block_offset_entry), "");
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

static void on_zoom_out_addr(GtkToolButton *button, gpointer user_data)
{
    viewer *v = (viewer *) user_data;
    viewer_region *vr = &v->region;

    g_object_freeze_notify(G_OBJECT(vr->dims[0].adj));
    update_zoom(&vr->dims[0], -1, 3, 2, false);
    g_object_thaw_notify(G_OBJECT(vr->dims[0].adj));
}

static void on_zoom_in_addr(GtkToolButton *button, gpointer user_data)
{
    viewer *v = (viewer *) user_data;
    viewer_region *vr = &v->region;

    g_object_freeze_notify(G_OBJECT(vr->dims[0].adj));
    update_zoom(&vr->dims[0], 1, 3, 4, false);
    g_object_thaw_notify(G_OBJECT(vr->dims[0].adj));
}

static void on_zoom_out_iseq(GtkToolButton *button, gpointer user_data)
{
    viewer *v = (viewer *) user_data;
    viewer_region *vr = &v->region;

    g_object_freeze_notify(G_OBJECT(vr->dims[1].adj));
    update_zoom(&vr->dims[1], -1, 3, 2, false);
    g_object_thaw_notify(G_OBJECT(vr->dims[1].adj));
}

static void on_zoom_in_iseq(GtkToolButton *button, gpointer user_data)
{
    viewer *v = (viewer *) user_data;
    viewer_region *vr = &v->region;

    g_object_freeze_notify(G_OBJECT(vr->dims[1].adj));
    update_zoom(&vr->dims[1], 1, 3, 4, false);
    g_object_thaw_notify(G_OBJECT(vr->dims[1].adj));
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
    update_region(&main_viewer.region);
    gtk_main();
    return 0;
}
