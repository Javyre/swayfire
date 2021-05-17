#ifndef SWAYFIRE_DECO_HPP
#define SWAYFIRE_DECO_HPP

#include <utility>
#include <wayfire/compositor-surface.hpp>
#include <wayfire/decorator.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/surface.hpp>

#include "../core/core.hpp"
#include "../core/plugin.hpp"
#include "subsurf.hpp"

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

    /// Whether the view is mapped or not.
    bool mapped = true;

    /// The loaded options from the cfg.
    nonstd::observer_ptr<Options> options;

    /// The current color set.
    nonstd::observer_ptr<DecorationColors> colors =
        &(options->colors.unfocused);

    /// The subsurfaces that make up the decoration.
    const struct {
        const RectSubSurf left, right, top, bottom;
        const CurveSubSurf top_left, top_right, bottom_left, bottom_right;
    } _subsurfs = {
        // Left side
        RectSubSurf(
            [&]() {
                return wf::geometry_t{
                    0,
                    options->border_radius,
                    options->border_width,
                    height - (2 * options->border_radius),
                };
            },
            [&]() { return colors->child_border.value(); }),

        // Right side
        RectSubSurf(
            [&]() {
                return wf::geometry_t{
                    width - options->border_width,
                    options->border_radius,
                    options->border_width,
                    height - (2 * options->border_radius),
                };
            },
            [&]() {
                return node->get_prefered_split_type() == SplitType::VSPLIT
                           ? colors->indicator.value()
                           : colors->child_border.value();
            }),

        // Top side
        RectSubSurf(
            [&]() {
                return wf::geometry_t{
                    options->border_radius,
                    0,
                    width - (2 * options->border_radius),
                    options->border_width,
                };
            },
            [&]() { return colors->child_border.value(); }),

        // Bottom side
        RectSubSurf(
            [&]() {
                return wf::geometry_t{
                    options->border_radius,
                    height - options->border_width,
                    width - (2 * options->border_radius),
                    options->border_width,
                };
            },
            [&]() {
                return node->get_prefered_split_type() == SplitType::HSPLIT
                           ? colors->indicator.value()
                           : colors->child_border.value();
            }),

        // Top-left corner
        CurveSubSurf(
            [&]() {
                return CurveSubSurf::Spec{
                    {options->border_radius, options->border_radius},
                    M_PI_2,
                    M_PI,
                    options->border_radius,
                    options->border_width,
                };
            },
            [&]() { return colors->child_border.value(); }),

        // Top-right corner
        CurveSubSurf(
            [&]() {
                return CurveSubSurf::Spec{
                    {width - options->border_radius, options->border_radius},
                    0,
                    M_PI_2,
                    options->border_radius,
                    options->border_width,
                };
            },
            [&]() { return colors->child_border.value(); }),

        // Bottom-left corner
        CurveSubSurf(
            [&]() {
                return CurveSubSurf::Spec{
                    {options->border_radius, height - options->border_radius},
                    M_PI,
                    M_PI + M_PI_2,
                    options->border_radius,
                    options->border_width,
                };
            },
            [&]() { return colors->child_border.value(); }),

        // Bottom-right corner
        CurveSubSurf(
            [&]() {
                return CurveSubSurf::Spec{
                    {width - options->border_radius,
                     height - options->border_radius},
                    M_PI + M_PI_2,
                    2 * M_PI,
                    options->border_radius,
                    options->border_width,
                };
            },
            [&]() { return colors->child_border.value(); }),
    };

    /// All subsurfaces in an array to easily iterate through them.
    const std::array<const ISubSurf *const, 8> subsurfs = {
        &_subsurfs.left,        &_subsurfs.right,
        &_subsurfs.top,         &_subsurfs.bottom,

        &_subsurfs.top_left,    &_subsurfs.top_right,
        &_subsurfs.bottom_left, &_subsurfs.bottom_right,
    };

    int width = 0;  ///< Width of the decoration.
    int height = 0; ///< Height of the decoration.

    wf::region_t cached_region; ///< Cached minimal region containing this deco.

    /// Calculate the minimal region that contains this decoration surface.
    wf::region_t calculate_region();

    wf::signal_connection_t on_prefered_split_type_changed =
        [&](wf::signal_data_t *) { damage(); };

    wf::signal_connection_t on_detached = [&](wf::signal_data_t *) {
        mapped = false;
        wf::emit_map_state_change(this);
        node->view->set_decoration(nullptr); // ViewDecoration dies here.
    };

  public:
    ViewDecoration(ViewNodeRef node, nonstd::observer_ptr<Options> options)
        : node(node), options(options) {

        node->connect_signal("prefered-split-type-changed",
                             &on_prefered_split_type_changed);
        node->connect_signal("detached", &on_detached);
        node->get_ws()->output->connect_signal("swf-deco-fini", &on_detached);

        cached_region = calculate_region();
    }

    ~ViewDecoration() override {
        node->get_ws()->output->disconnect_signal(&on_detached);
        node->disconnect_signal(&on_detached);
        node->disconnect_signal(&on_prefered_split_type_changed);
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
    bool accepts_input(int32_t sx, int32_t sy) override;
    void simple_render(const wf::framebuffer_t &fb, int x, int y,
                       const wf::region_t &damage) override;
};

class SwayfireDeco : public SwayfirePlugin {
  private:
    /// Add decorations to the node.
    void decorate_node(Node node);

    wf::signal_connection_t on_view_node_attached =
        [&](wf::signal_data_t *data) {
            auto vnode = get_signaled_view_node(data);
            decorate_node(vnode);
        };

    Options options{};

  public:
    // == Impl SwayfirePlugin ==
    void swf_init() override;
    void swf_fini() override;
};

#endif // ifndef SWAYFIRE_DECO_HPP
