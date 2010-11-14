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
#include <memory>

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include "dg_record.h"

using namespace std;

/* A single tree view showing a stack trace. */
struct stack_trace_view
{
    GtkWidget *view;        /* GtkTreeView */
    GtkListStore *store;
};

/* Column numbers in the model backing a stack_trace_view */
enum stack_trace_column
{
    STC_ADDR = 0,
    STC_FUNCTION,
    STC_FILE,
    STC_LINE,
    STC_DSO
};

struct viewer;

/* A single viewport onto the space-time. Currently there is only one, but
 * the design allows for more.
 */
class viewer_region
{
public:
    /* Captures the viewable range of one axis. Coordinates are in the
     * abstract space (e.g. instruction count or remapped address).
     */
    class dimension
    {
    private:
        /* The cached values indicate what the parent's GtkImage currently
         * hold. They are updated when the image is updated, and compared
         * to the adjustment.
         */
        double cached_lower;
        double cached_upper;

        /* Adjustment for the scroll bar controlling this view region.
         * This pointer holds a GObject ref.
         */
        GtkAdjustment *adj;

        // Prevent copying, since it will mess up the refcounting
        dimension(const dimension &);
        dimension &operator=(const dimension &);
    public:
        dimension() : cached_lower(-1.0), cached_upper(-1.0), adj(NULL)
        {
            // The initializers mark the cache as dirty
        }

        double range() const
        {
            return gtk_adjustment_get_page_size(adj);
        }

        double lower() const
        {
            return gtk_adjustment_get_value(adj);
        }

        double upper() const
        {
            return lower() + range();
        }

        GtkAdjustment *get_adjustment() const
        {
            return adj;
        }

        /* Invalidates the cache and sets the adjustment. Should be called
         * immediately after construction (it is not a constructor because
         * that would interfere with embedding it inside the parent, as C++98
         * doesn't allow array members to be initialized.
         */
        void init(viewer_region *owner, GtkAdjustment *adj);

        bool cache_dirty() const
        {
            return cached_lower != lower() || cached_upper != upper();
        }

        void update_cache()
        {
            cached_lower = lower();
            cached_upper = upper();
        }

        void update(int w1, int w2, int size, bool massage);

        void freeze()
        {
            g_object_freeze_notify(G_OBJECT(adj));
        }

        void thaw()
        {
            g_object_thaw_notify(G_OBJECT(adj));
        }

        ~dimension()
        {
            if (this->adj != NULL)
                g_object_unref(this->adj);
        }
    };

private:
    viewer *owner;

    GtkWidget *image;   /* GtkImage */
    GtkWidget *events;  /* GtkEventBox */
    GtkWidget *top;     /* Top-level widget to add to parents */
    GdkPixbuf *pixbuf;

    /* These are set when the click depresses the mouse, and used during
     * mouse release to distinguish a drag (for zoom) vs click (for info).
     */
    int click_x;
    int click_y;
    bool in_click;

    dimension dims[2];

    /* Prevent copying */
    viewer_region &operator=(const viewer_region &);
    viewer_region(const viewer_region &);

    /* Draws the vertical grid lines showing cache line and page line
     * boundaries. Should only be called from update().
     */
    void update_lines();

public:
    viewer *get_owner() const { return owner; }
    GtkWidget *get_widget() const { return top; }
    dimension *get_addr_dimension() { return &dims[0]; }
    dimension *get_iseq_dimension() { return &dims[1]; }

    /* Unconditionally regenerates the backing pixmap. It does not check
     * check whether an update is necessary, and it does not handle resizes.
     */
    void update();

    void on_changed(GtkAdjustment *adj);
    void on_press(GtkWidget *widget, GdkEventButton *event);
    void on_release(GtkWidget *widget, GdkEventButton *event);
    void on_resize(GtkWidget *widget, GtkAllocation *event);

    viewer_region() : owner(NULL), image(NULL), events(NULL), top(NULL), pixbuf(NULL)
    {
    }
    ~viewer_region();

    void init(viewer *v, int width, int height,
              GtkAdjustment *addr_adj, GtkAdjustment *iseq_adj);
};

/* Container for the main window and all its state */
struct viewer
{
    dg_view_base *accesses;

    GtkWidget *window;             /* GtkWindow */
    viewer_region region;

    /* The adjustments for the zoomable regions. These do NOT hold references,
     * as there are sufficient references in the viewer_region and the widget
     * tree to prevent them from vanishing.
     */
    GtkAdjustment *addr_adj;
    GtkAdjustment *iseq_adj;

    GtkWidget *addr_entry;         /* GtkEntry - displays access address */
    GtkWidget *block_addr_entry;   /* GtkEntry - displays base of memory block */
    GtkWidget *block_size_entry;   /* GtkEntry - displays size of memory block */
    GtkWidget *block_offset_entry; /* GtkEntry - displays offset of address in block */

    stack_trace_view access_stack; /* Stack trace where the access occurred */
    stack_trace_view block_stack;  /* Stack trace where the memory was allocated */
};

/* Wrappers around the on_* methods for passing as callback functions. */

static gboolean viewer_region_on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean viewer_region_on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean viewer_region_on_resize(GtkWidget *widget, GtkAllocation *event, gpointer user_data);
static void viewer_region_on_changed(GtkAdjustment *adj, gpointer user_data);
static void viewer_region_dimension_on_zoom_in(GtkAction *action, gpointer user_data);
static void viewer_region_dimension_on_zoom_out(GtkAction *action, gpointer user_data);

void viewer_region::dimension::init(viewer_region *owner, GtkAdjustment *adj)
{
    g_return_if_fail(owner != NULL);
    g_return_if_fail(adj != NULL);
    g_return_if_fail(this->adj == NULL);

    g_object_ref(adj);
    this->adj = adj;

    g_signal_connect(G_OBJECT(adj), "changed",
                     G_CALLBACK(viewer_region_on_changed), owner);
    g_signal_connect(G_OBJECT(adj), "value-changed",
                     G_CALLBACK(viewer_region_on_changed), owner);
}

/* Computes new values for dimension, based on two coordinates of a zoom box in
 * a window space [0, size).
 *
 * If massage is true, the zoom area is expanded slightly then clamped. It
 * should be true when doing click-and-drag zooming, and false when zooming
 * in response to toolbar buttons.
 *
 * This method should be called between freeze() and thaw().
 */
void viewer_region::dimension::update(int w1, int w2, int size, bool massage)
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
    double old_size = range();
    double low = lower() + l * old_size;
    double high = lower() + h * old_size;

    /* Clamp to maximum range of adjustment */
    low = max(low, gtk_adjustment_get_lower(adj));
    high = min(high, gtk_adjustment_get_upper(adj));

    double page_size = high - low;
    gtk_adjustment_set_page_size(adj, page_size);
    gtk_adjustment_set_page_increment(adj, page_size);
    gtk_adjustment_set_step_increment(adj, 0.1 * page_size);
    gtk_adjustment_set_value(adj, low);
}

void viewer_region::init(viewer *v, int width, int height,
                         GtkAdjustment *addr_adj, GtkAdjustment *iseq_adj)
{
    g_assert(owner == NULL);
    owner = v;

    pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    gdk_pixbuf_fill(pixbuf, 0x00000000);

    image = gtk_image_new_from_pixbuf(pixbuf);
    g_object_unref(pixbuf); /* image will hold a ref for us */
    gtk_widget_set_size_request(image, width, height);

    events = gtk_event_box_new();
    gtk_widget_add_events(events, GDK_BUTTON_PRESS_MASK);
    gtk_container_add(GTK_CONTAINER(events), image);

    g_signal_connect(G_OBJECT(events), "button-press-event",
                     G_CALLBACK(viewer_region_on_press), this);
    g_signal_connect(G_OBJECT(events), "button-release-event",
                     G_CALLBACK(viewer_region_on_release), this);
    g_signal_connect(G_OBJECT(events), "size-allocate",
                     G_CALLBACK(viewer_region_on_resize), this);

    dims[0].init(this, addr_adj);
    dims[1].init(this, iseq_adj);

    GtkWidget *hscroll = gtk_hscrollbar_new(addr_adj);
    GtkWidget *vscroll = gtk_vscrollbar_new(iseq_adj);
    top = gtk_table_new(2, 2, FALSE);
    gtk_table_attach(GTK_TABLE(top), events, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     0, 0);
    gtk_table_attach(GTK_TABLE(top), hscroll, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     (GtkAttachOptions) 0,
                     0, 0);
    gtk_table_attach(GTK_TABLE(top), vscroll, 1, 2, 0, 1,
                     (GtkAttachOptions) 0,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                     0, 0);
}

viewer_region::~viewer_region()
{
}

/* Display an address in hex, and abbreviate filenames */
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
    g_object_unref(store); /* Filter holds a ref */
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
    address_type addr_min, addr_max;
    iseq_type iseq_min, iseq_max;
    v->accesses->get_ranges(addr_min, addr_max, iseq_min, iseq_max);

    v->addr_adj = GTK_ADJUSTMENT(gtk_adjustment_new(addr_min, addr_min, addr_max,
                                                    addr_max - addr_min,
                                                    0.1 * (addr_max - addr_min),
                                                    addr_max - addr_min));
    v->iseq_adj = GTK_ADJUSTMENT(gtk_adjustment_new(iseq_min, iseq_min, iseq_max,
                                                    iseq_max - iseq_min,
                                                    0.1 * (iseq_max - iseq_min),
                                                    iseq_max - iseq_min));
}

static void build_toolbar_section(GtkToolbar *toolbar, viewer_region::dimension *dim)
{
    GtkToolItem *button;

    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_IN);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(viewer_region_dimension_on_zoom_in), dim);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);
    button = gtk_tool_button_new_from_stock(GTK_STOCK_ZOOM_OUT);
    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(viewer_region_dimension_on_zoom_out), dim);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), button, -1);
}

static GtkWidget *build_toolbar(viewer *v)
{
    GtkWidget *toolbar;

    toolbar = gtk_toolbar_new();
    viewer_region::dimension *addr_dim = v->region.get_addr_dimension();
    viewer_region::dimension *iseq_dim = v->region.get_iseq_dimension();

    build_toolbar_section(GTK_TOOLBAR(toolbar), addr_dim);
    gtk_toolbar_insert(GTK_TOOLBAR(toolbar), gtk_separator_tool_item_new(), -1);
    build_toolbar_section(GTK_TOOLBAR(toolbar), iseq_dim);
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

    v->region.init(v, 600, 600, v->addr_adj, v->iseq_adj);
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
    gtk_table_attach_defaults(GTK_TABLE(table), v->region.get_widget(), 0, 1, 0, 1);

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

void viewer_region::update_lines()
{
    int rowstride = gdk_pixbuf_get_rowstride(pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(pixbuf);
    int width = gdk_pixbuf_get_width(pixbuf);
    int height = gdk_pixbuf_get_height(pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(pixbuf) == 8);

    const double addr_min = dims[0].lower();
    const double addr_max = dims[0].upper();
    const double xrate = dims[0].range() / width;
    const uint8_t color_cut[3] = {192, 192, 192}; // light gray
    const uint8_t color_page[3] = {64, 64, 64};   // dark gray
    const uint8_t color_cache[3] = {96, 32, 32};  // reddish
    int x;
    address_type last = 0;

    dg_view_base::forward_page_map_iterator i, pm_first, pm_last;
    pm_first = owner->accesses->page_map_begin();
    pm_last = owner->accesses->page_map_end();
    for (i = pm_first; i != pm_last; ++i)
    {
        if (i->first != last + DG_VIEW_PAGE_SIZE)
        {
            // Address map discontinuity
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

void viewer_region::update()
{
    int rowstride = gdk_pixbuf_get_rowstride(pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(pixbuf);
    int width = gdk_pixbuf_get_width(pixbuf);
    int height = gdk_pixbuf_get_height(pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(pixbuf) == 8);
    gdk_pixbuf_fill(pixbuf, 0);

    update_lines();

    const double addr_min = dims[0].lower();
    const double addr_max = dims[0].upper();
    const double iseq_min = dims[1].lower();
    const double iseq_max = dims[1].upper();

    for (dg_view_base::const_iterator a = owner->accesses->begin(); a != owner->accesses->end(); ++a)
    {
        address_type orig_addr = a->get_addr();
        size_t addr = owner->accesses->remap_address(orig_addr);
        iseq_type iseq = a->get_iseq();
        int x1, y1, x2, y2;

        if (range_to_pixels(addr, addr + a->get_size(),
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
                    switch (a->get_dir())
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

    gtk_widget_queue_draw(image);

    for (int i = 0; i < 2; i++)
        dims[i].update_cache();
}

void viewer_region::on_changed(GtkAdjustment *adj)
{
    /* Note that zooming can cause multiple signals to be emitted, but we
     * only need to redraw once. Hence the cache.
     */
    if (dims[0].cache_dirty() || dims[1].cache_dirty())
        update();
}

static void viewer_region_on_changed(GtkAdjustment *adj, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;
    vr->on_changed(adj);
}

void viewer_region::on_press(GtkWidget *widget, GdkEventButton *event)
{
    in_click = true;
    click_x = event->x;
    click_y = event->y;
    gtk_grab_add(widget);
}

static gboolean viewer_region_on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;
    vr->on_press(widget, event);
    return FALSE;
}

static void stack_trace_view_populate(stack_trace_view *stv, const vector<address_type> &st)
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

void viewer_region::on_release(GtkWidget *widget, GdkEventButton *event)
{
    gtk_grab_remove(widget);

    if (in_click)
    {
        in_click = false;
        int width = gdk_pixbuf_get_width(pixbuf);
        int height = gdk_pixbuf_get_height(pixbuf);
        if (abs(event->x - click_x) > 2 && abs(event->y - click_y) > 2)
        {
            /* Assume it was a drag to zoom */
            dims[0].freeze();
            dims[1].freeze();
            dims[0].update(click_x, event->x, width, true);
            dims[1].update(click_y, event->y, width, true);
            dims[0].thaw();
            dims[1].thaw();
        }
        else
        {
            /* A click to get information */
            const double addr_min = dims[0].lower();
            const double addr_size = dims[0].range();
            const double iseq_min = dims[1].lower();
            const double iseq_size = dims[1].range();

            double addr_scale = addr_size / width;
            double iseq_scale = iseq_size / height;
            double ratio = iseq_scale / addr_scale;

            size_t remapped = (size_t) (0.5 + (event->x + 0.5) * addr_scale + addr_min);
            address_type addr = owner->accesses->revmap_addr(remapped);
            double iseq = (event->y + 0.5) * iseq_scale + iseq_min;

            dg_view_base::const_iterator access = owner->accesses->nearest_access(addr, iseq, ratio);
            if (access != owner->accesses->end())
            {
                ostringstream addr_str;
                addr_str << showbase << hex << access->get_addr();
                gtk_entry_set_text(GTK_ENTRY(owner->addr_entry), addr_str.str().c_str());

                if (access->get_mem_size() != 0)
                {
                    addr_str.str("");
                    addr_str << access->get_mem_addr();
                    gtk_entry_set_text(GTK_ENTRY(owner->block_addr_entry), addr_str.str().c_str());

                    ostringstream size_str;
                    size_str << access->get_mem_size();
                    gtk_entry_set_text(GTK_ENTRY(owner->block_size_entry), size_str.str().c_str());

                    ostringstream offset_str;
                    offset_str << access->get_addr() - access->get_mem_addr();
                    gtk_entry_set_text(GTK_ENTRY(owner->block_offset_entry), offset_str.str().c_str());

                    stack_trace_view_populate(&owner->block_stack, access->get_mem_stack());
                }
                else
                {
                    /* Clear it */
                    gtk_entry_set_text(GTK_ENTRY(owner->block_addr_entry), "");
                    gtk_entry_set_text(GTK_ENTRY(owner->block_size_entry), "");
                    gtk_entry_set_text(GTK_ENTRY(owner->block_offset_entry), "");
                    stack_trace_view_populate(&owner->block_stack, vector<address_type>());
                }

                stack_trace_view_populate(&owner->access_stack, access->get_stack());
            }
        }
    }
}

static gboolean viewer_region_on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;
    vr->on_release(widget, event);
    return FALSE;
}

void viewer_region::on_resize(GtkWidget *widget, GtkAllocation *event)
{
    if (event->width != gdk_pixbuf_get_width(pixbuf)
        || event->height != gdk_pixbuf_get_height(pixbuf))
    {
        /* Pixbuf cannot be resized in place, so allocate a new one and
         * deref the old.
         */
        GdkPixbuf *new_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8,
                                               event->width, event->height);
        gtk_image_set_from_pixbuf(GTK_IMAGE(image), new_pixbuf);
        pixbuf = new_pixbuf;
        g_object_unref(new_pixbuf); /* The GtkImage holds a ref for us */
        update();
    }
}

static gboolean viewer_region_on_resize(GtkWidget *widget, GtkAllocation *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;
    vr->on_resize(widget, event);
    return FALSE;
}

static void viewer_region_dimension_on_zoom_out(GtkAction *action, gpointer user_data)
{
    viewer_region::dimension *d = (viewer_region::dimension *) user_data;
    d->update(-1, 3, 2, false); // Doubles viewport
}

static void viewer_region_dimension_on_zoom_in(GtkAction *action, gpointer user_data)
{
    viewer_region::dimension *d = (viewer_region::dimension *) user_data;
    d->update(1, 3, 4, false);  // Halves viewport
}

int main(int argc, char **argv)
{
    gtk_init(&argc, &argv);
    dg_view_options options;

    options.parse_opts(&argc, argv);
    if (argc != 2)
    {
        dg_view_usage(argv[0], 2);
    }

    auto_ptr<dg_view_base> accesses(dg_view_load(argv[1], options));
    if (accesses.get() == NULL)
    {
        return 1;
    }

    viewer main_viewer;
    main_viewer.accesses = accesses.get();
    build_main_window(&main_viewer);
    main_viewer.region.update();
    gtk_main();
    return 0;
}
