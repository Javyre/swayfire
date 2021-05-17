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

/// Renderable subsurface interface.
struct ISubSurf {
    virtual ~ISubSurf() = default;
    virtual void render(wf::point_t origin, glm::mat4 matrix) const = 0;
    [[nodiscard]] virtual wf::region_t calculate_region() const = 0;
    [[nodiscard]] virtual bool contains_point(wf::point_t pt) const = 0;
};

/// Simple Rectangle subsurface.
struct RectSubSurf : public ISubSurf {
    using GeoFun = std::function<wf::geometry_t()>;
    using ColorFun = std::function<wf::color_t()>;

    const GeoFun get_geo;
    const ColorFun get_color;

    RectSubSurf(GeoFun get_geo, ColorFun get_color)
        : get_geo(std::move(get_geo)), get_color(std::move(get_color)) {}

    void render(wf::point_t origin, glm::mat4 matrix) const override;
    [[nodiscard]] wf::region_t calculate_region() const override;
    [[nodiscard]] bool contains_point(wf::point_t pt) const override;
};

/// Curved line subsurface.
struct CurveSubSurf : public ISubSurf {
    struct Spec {
        wf::point_t origin; ///< Origin for the arc radius.
        float theta_a;      ///< From angle.
        float theta_b;      ///< To angle.
        int radius;         ///< Outer radius of curved line.
        int stroke_width;   ///< Width of the curved line.
    };

    using SpecFun = std::function<Spec()>;
    using ColorFun = std::function<wf::color_t()>;

    const SpecFun get_spec;
    const ColorFun get_color;

    CurveSubSurf(SpecFun get_spec, ColorFun get_color)
        : get_spec(std::move(get_spec)), get_color(std::move(get_color)) {}

    void render(wf::point_t origin, glm::mat4 matrix) const override;
    [[nodiscard]] wf::region_t calculate_region() const override;
    [[nodiscard]] bool contains_point(wf::point_t pt) const override;
};

#endif // ifndef SWAYFIRE_SUBSURF_HPP
