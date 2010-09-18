#include <gtk/gtk.h>
#include <glib.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <cstdlib>
#include <cmath>

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include "dg_record.h"

using namespace std;

struct viewer_region
{
    GtkWidget *image;   /* GtkImage */
    GtkWidget *events;  /* GtkEventBox */
    GdkPixbuf *pixbuf;

    float addr_min, addr_max;
    float iseq_min, iseq_max;
};

struct viewer
{
    GtkWidget *window; /* GtkWindow */
    viewer_region region;
};

static gboolean on_click(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    /* TODO */
    viewer_region *vr = (viewer_region *) user_data;
    return FALSE;
}

static void build_region(viewer_region &vr, int width, int height)
{
    vr.pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    if (vr.pixbuf == NULL)
    {
        fprintf(stderr, "Could not allocate a %d x %d pixbuf\n", width, height);
        exit(1);
    }

    vr.image = gtk_image_new_from_pixbuf(vr.pixbuf);
    if (vr.image == NULL)
    {
        fprintf(stderr, "Could not allocate an image for the pixbuf\n");
        exit(1);
    }

    vr.events = gtk_event_box_new();
    if (vr.events == NULL)
    {
        fprintf(stderr, "Could not allocate an event box\n");
        exit(1);
    }
    gtk_widget_add_events(vr.events, GDK_BUTTON_PRESS_MASK);
    g_signal_connect(G_OBJECT(vr.events), "button-press-event",
                     G_CALLBACK(on_click), &vr);
    gtk_container_add(GTK_CONTAINER(vr.events), vr.image);

    gtk_widget_set_size_request(vr.image, width, height);

    gdk_pixbuf_fill(vr.pixbuf, 0x00000000);
}

static void build_main_window(viewer &v)
{
    v.window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(v.window), "dg_view");
    g_signal_connect(G_OBJECT(v.window), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);

    build_region(v.region, 800, 800);
    gtk_container_add(GTK_CONTAINER(v.window), v.region.events);

    gtk_widget_show_all(v.window);
}

static void prepare_min_max(viewer_region &vr)
{
    const bbrun_list &bbruns = dg_view_bbruns();
    const page_map &pages = dg_view_page_map();
    vr.addr_min = pages.begin()->second;
    vr.addr_max = (--pages.end())->second + DG_VIEW_PAGE_SIZE;
    vr.iseq_min = 0;

    bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = dg_view_bbrun_get_bbdef(*last_bbrun);
    vr.iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;
}

/* Takes a range in a semantic space and gives first and last pixels covered.
 * The return values are a [first, last) range. Returns false if there is
 * no intersection with the viewport.
 */
static bool range_to_pixels(float lo, float hi,
                            float view_min, float view_max, int pixels,
                            int &pixel_lo, int &pixel_hi)
{
    if (hi <= view_min || lo >= view_max)
        return false;
    float scale = pixels / (view_max - view_min);
    pixel_lo = (int) floor((lo - view_min) * scale + 0.5f);
    pixel_hi = (int) floor((hi - view_min) * scale + 0.5f);
    if (pixel_hi == pixel_lo)
        pixel_hi++;
    pixel_lo = max(pixel_lo, 0);
    pixel_hi = min(pixel_hi, pixels);
    return true;
}

static void update_region(viewer_region &vr)
{
    int rowstride = gdk_pixbuf_get_rowstride(vr.pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(vr.pixbuf);
    int width = gdk_pixbuf_get_width(vr.pixbuf);
    int height = gdk_pixbuf_get_height(vr.pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(vr.pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(vr.pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(vr.pixbuf) == 8);
    gdk_pixbuf_fill(vr.pixbuf, 0);

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
                                    vr.addr_min, vr.addr_max, width,
                                    x1, x2) &&
                    range_to_pixels(iseq, iseq + 1,
                                    vr.iseq_min, vr.iseq_max, height,
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

    gtk_widget_queue_draw(vr.image);
}

int main(int argc, char **argv)
{
    viewer main_viewer;
    g_thread_init(NULL);
    gtk_init(&argc, &argv);

    dg_view_parse_opts(&argc, argv);
    if (argc != 2)
    {
        dg_view_usage(argv[0], 2);
    }
    if (!dg_view_load(argv[1]))
        return 1;

    build_main_window(main_viewer);
    prepare_min_max(main_viewer.region);
    update_region(main_viewer.region);
    gtk_main();
    return 0;
}
