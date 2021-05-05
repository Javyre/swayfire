#ifndef SWAYFIRE_DECO_HPP
#define SWAYFIRE_DECO_HPP

#include <utility>
#include <wayfire/compositor-surface.hpp>
#include <wayfire/decorator.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/surface.hpp>

#include "../core/core.hpp"
#include "../core/plugin.hpp"

struct DecorationColors {
    wf::option_wrapper_t<wf::color_t> border, background, text, indicator,
        child_border;
};

struct Options {
    wf::option_wrapper_t<int> border_width{"swayfire-deco/border_width"};
    wf::option_wrapper_t<int> border_radius{"swayfire-deco/border_radius"};
    wf::option_wrapper_t<bool> title_bar{"swayfire-deco/title_bar"};

    struct DecoColorSets {
        /// Focused deco color set.
        DecorationColors focused{
            {"swayfire-deco/focused.border"},
            {"swayfire-deco/focused.background"},
            {"swayfire-deco/focused.text"},
            {"swayfire-deco/focused.indicator"},
            {"swayfire-deco/focused.child_border"},
        };
        /// Focused-inactive deco color set.
        DecorationColors focused_inactive{
            {"swayfire-deco/focused_inactive.border"},
            {"swayfire-deco/focused_inactive.background"},
            {"swayfire-deco/focused_inactive.text"},
            {"swayfire-deco/focused_inactive.indicator"},
            {"swayfire-deco/focused_inactive.child_border"},
        };
        /// Unfocused deco color set.
        DecorationColors unfocused{
            {"swayfire-deco/unfocused.border"},
            {"swayfire-deco/unfocused.background"},
            {"swayfire-deco/unfocused.text"},
            {"swayfire-deco/unfocused.indicator"},
            {"swayfire-deco/unfocused.child_border"},
        };

        // TODO: implement other i3 class colors
    } colors;
};

class ViewDecoration : public wf::compositor_surface_t,
                       public wf::surface_interface_t,
                       public wf::decorator_frame_t_t {
  private:
    const ViewNodeRef node; ///< The node we're decorating.

    /// The loaded options from the cfg.
    nonstd::observer_ptr<Options> options;

    /// The current color set.
    nonstd::observer_ptr<DecorationColors> colors =
        &(options->colors.unfocused);

    int width = 0;  ///< Width of the decoration.
    int height = 0; ///< Height of the decoration.

    wf::signal_connection_t on_prefered_split_type_changed =
        [&](wf::signal_data_t *) { damage(); };

  public:
    ViewDecoration(ViewNodeRef node, nonstd::observer_ptr<Options> options)
        : node(node), options(options) {
        node->connect_signal("prefered-split-type-changed",
                             &on_prefered_split_type_changed);
    }

    /// Damage the decoration region.
    void damage();

    // == Impl wf::decorator_frame_t_t ==
    wf::geometry_t expand_wm_geometry(wf::geometry_t content) override;
    void calculate_resize_size(int &target_width, int &target_height) override;
    void notify_view_activated(bool active) override;
    void notify_view_resized(wf::geometry_t view_geometry) override;
    /* TODO: impl these handlers
    void notify_view_tiled() override;
    void notify_view_fullscreen() override;
    */

    // == Impl wf::surface_interface_t ==
    bool is_mapped() const override;
    wf::point_t get_offset() override;
    wf::dimensions_t get_size() const override;
    void simple_render(const wf::framebuffer_t &fb, int x, int y,
                       const wf::region_t &damage) override;
};

class SwayfireDeco : public SwayfirePlugin {
  private:
    /// Add decorations to the node.
    void decorate_node(Node node);

    wf::signal_connection_t on_view_node_attached =
        [&](wf::signal_data_t *data) {
            get_signaled_view_node(data)->view->connect_signal(
                "mapped", &on_view_first_map);
        };

    wf::signal_connection_t on_view_first_map = [&](wf::signal_data_t *data) {
        decorate_node(get_signaled_view_node(data));

        get_signaled_view_node(data)->view->disconnect_signal(
            &on_view_first_map);
    };

    Options options{};

  public:
    // == Impl SwayfirePlugin ==
    void swf_init() override;
    void swf_fini() override;
};

#endif // ifndef SWAYFIRE_DECO_HPP
