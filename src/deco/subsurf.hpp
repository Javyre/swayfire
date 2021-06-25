#ifndef SWAYFIRE_SUBSURF_HPP
#define SWAYFIRE_SUBSURF_HPP

#include <functional>
#include <glm/ext/matrix_float4x4.hpp>
#include <utility>
#include <wayfire/config/types.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/util.hpp>

/// Initialize the gl programs.
extern void subsurf_gl_init();

/// Stop using the gl programs.
extern void subsurf_gl_fini();

/// Simple Rectangle subsurface.
namespace RectSubSurf {
using Spec = wf::geometry_t;
void render(wf::geometry_t geo, wf::color_t color, wf::point_t origin,
            glm::mat4 matrix);
[[nodiscard]] wf::region_t calculate_region(wf::geometry_t geo);
[[nodiscard]] bool contains_point(wf::geometry_t geo, wf::point_t pt);
}; // namespace RectSubSurf

/// Curved line subsurface.
namespace CurveSubSurf {
struct Spec {
    wf::point_t origin; ///< Origin for the arc radius.
    float theta_a;      ///< From angle.
    float theta_b;      ///< To angle.
    int radius;         ///< Outer radius of curved line.
    int stroke_width;   ///< Width of the curved line.
};

void render(Spec spec, wf::color_t color, wf::point_t origin, glm::mat4 matrix);
[[nodiscard]] wf::region_t calculate_region(Spec spec);
[[nodiscard]] bool contains_point(Spec spec, wf::point_t pt);
}; // namespace CurveSubSurf

/// Border subsurface.
namespace BorderSubSurf {
struct Spec {
    wf::geometry_t geo; ///< Bounding box of the frame.
    int border_radius;  ///< Outer radius of the corners.
    int border_width;   ///< Width of the border.
};

struct Colors {
    wf::color_t all, right, bottom;
};

void render(Spec spec, Colors colors, wf::point_t origin, glm::mat4 matrix);
[[nodiscard]] wf::region_t calculate_region(Spec spec);
[[nodiscard]] bool contains_point(Spec spec, wf::point_t pt);
}; // namespace BorderSubSurf

#endif // ifndef SWAYFIRE_SUBSURF_HPP
