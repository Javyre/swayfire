#include "subsurf.hpp"

#include <wayfire/opengl.hpp>
#include <wayfire/plugins/common/cairo-util.hpp>

// Shaders

static const char *const curve_vert = R"(#version 100
attribute mediump vec2 position;
varying mediump vec2 fposition;

uniform mat4 matrix;

void main() {
    gl_Position = matrix * vec4(position, 0.0, 1.0);
    fposition = position;
})";

static const char *const curve_frag = R"(#version 100
@builtin_ext@
varying mediump vec2 fposition;
@builtin@

uniform mediump vec2 origin;
uniform mediump float theta_a;
uniform mediump float theta_b;
uniform mediump float radius;
uniform mediump float stroke_width;
uniform mediump vec4 color;

#define epsilon 0.4

void main() {
    mediump float dist = distance(fposition, origin);
    mediump float inner_radius = radius - stroke_width;
    if (dist > radius + epsilon || dist < inner_radius - epsilon)
        discard;

    mediump vec2 rel = fposition - origin;
    mediump float angle = atan(rel.y, -rel.x);

    if (angle < theta_a || angle > theta_b)
        discard;

    gl_FragColor = color;
    gl_FragColor *= 1.0 - smoothstep(radius - epsilon, 
                                     radius + epsilon,
                                     dist);
    gl_FragColor *= smoothstep(inner_radius - epsilon, 
                               inner_radius + epsilon, 
                               dist);
})";

/// Curve glsl program compiled once only.
static OpenGL::program_t curve_program{};
/// Whether the gl programs have been compiled yet.
static bool gl_compiled = false;
/// Whether the gl programs are still in use.
static int gl_in_use = 0;

void subsurf_gl_init() {
    if (!gl_compiled) {
        assert(!gl_in_use);

        OpenGL::render_begin();
        curve_program.compile(curve_vert, curve_frag);
        OpenGL::render_end();

        gl_compiled = true;
    }
    gl_in_use++;
}

void subsurf_gl_fini() {
    gl_in_use--;
    if (!gl_in_use) {
        OpenGL::render_begin();
        curve_program.free_resources();
        OpenGL::render_end();
    }
}

// RectSubSurf

namespace RectSubSurf {
void render(wf::geometry_t geo, wf::color_t color, wf::point_t origin,
            glm::mat4 matrix) {
    OpenGL::render_rectangle(geo + origin, color, matrix);
}

wf::region_t calculate_region(wf::geometry_t geo) { return geo; }

bool contains_point(wf::geometry_t geo, wf::point_t pt) {
    return wf::region_t{geo}.contains_point(pt);
}
} // namespace RectSubSurf

// CurveSubSurf

namespace CurveSubSurf {
void render(Spec spec, wf::color_t color, wf::point_t origin,
            glm::mat4 matrix) {
    if (spec.radius == 0 || spec.stroke_width == 0 ||
        spec.theta_a == spec.theta_b)
        return;

    origin = origin + spec.origin;

    curve_program.use(wf::TEXTURE_TYPE_RGBA);
    auto x = (float)(origin.x - spec.radius);
    auto y = (float)(origin.y - spec.radius);
    auto w = (float)(2 * spec.radius);
    auto h = (float)(2 * spec.radius);

    // We draw a square and let the fragment shader remove the irrelevant
    // pixels.
    GLfloat vertexData[] = {
        x,     y + h, //
        x + w, y + h, //
        x + w, y,     //
        x,     y,
    };

    curve_program.attrib_pointer("position", 2, 0, vertexData);
    curve_program.uniformMatrix4f("matrix", matrix);
    curve_program.uniform2f("origin", (float)origin.x, (float)origin.y);
    // -M_PI to convert from math [0, 2*pi] to GLSL's [-pi, pi] range for
    // atan()
    curve_program.uniform1f("theta_a", spec.theta_a - (float)M_PI);
    curve_program.uniform1f("theta_b", spec.theta_b - (float)M_PI);
    curve_program.uniform1f("radius", (float)spec.radius);
    curve_program.uniform1f("stroke_width", (float)spec.stroke_width);
    curve_program.uniform4f("color", {color.r, color.g, color.b, color.a});

    GL_CALL(glEnable(GL_BLEND));
    GL_CALL(glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA));
    GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));

    curve_program.deactivate();
}

wf::region_t calculate_region(Spec spec) {
    return wf::geometry_t{
        spec.origin.x - spec.radius,
        spec.origin.y - spec.radius,
        2 * spec.radius,
        2 * spec.radius,
    };
}

bool contains_point(Spec spec, wf::point_t pt) {
    const wf::point_t diff = pt - spec.origin;
    const float dist = std::sqrt((float)(diff.x * diff.x + diff.y * diff.y));

    const int inner_radius = spec.radius - spec.stroke_width;

    if (dist > (float)spec.radius || dist < (float)inner_radius)
        return false;

    const float angle = std::atan2(diff.y, -diff.x) + M_PI;

    if (angle < spec.theta_a || angle > spec.theta_b)
        return false;

    return true;
}
} // namespace CurveSubSurf

// TextSubSurf

void TextSubSurf::cache_texture(CachedSpec spec) {
    constexpr auto format = CAIRO_FORMAT_ARGB32;
    auto surface =
        cairo_image_surface_create(format, spec.size.width, spec.size.height);
    auto cr = cairo_create(surface);

    constexpr float font_scale = 0.8;
    const float font_size = (float)spec.size.height * font_scale;

    // render text
    const std::string font(spec.font);
    cairo_select_font_face(cr, font.c_str(), CAIRO_FONT_SLANT_NORMAL,
                           CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_source_rgba(cr, spec.color.r, spec.color.g, spec.color.b,
                          spec.color.a);

    cairo_set_font_size(cr, font_size);
    cairo_move_to(cr, 0, font_size);

    /* cairo_text_extents_t ext; */
    const std::string text(spec.text);
    /* cairo_text_extents(cr, text.c_str(), &ext); */
    cairo_show_text(cr, text.c_str());
    cairo_destroy(cr);

    cairo_surface_upload_to_texture(surface, texture);
    cairo_surface_destroy(surface);
}

void TextSubSurf::render(Spec spec, wf::point_t origin,
                         glm::mat4 matrix) const {
    const wf::geometry_t geo = {
        spec.x + origin.x,
        spec.y + origin.y,
        texture.width,
        texture.height,
    };

    // TODO: see if we can instead set the color using this color multiplier
    // parameter. That way we don't have to invalidate the cached texture every
    // time we change the color.
    OpenGL::render_transformed_texture(texture.tex, geo, matrix,
                                       glm::vec4(1.0f),
                                       OpenGL::TEXTURE_TRANSFORM_INVERT_Y);
}

wf::region_t TextSubSurf::calculate_region(Spec spec) const {
    (void)spec;
    return {};
}
bool TextSubSurf::contains_point(Spec spec, wf::point_t pt) const {
    (void)spec;
    (void)pt;
    return false;
}

// RoundedRectSubSurf

namespace RoundedRectSubSurf {
struct SubSpecs {
    const RectSubSurf::Spec left, right, top, bottom, center;
    const CurveSubSurf::Spec top_left, top_right, bottom_left, bottom_right;
};

inline bool is_rect(Spec spec) {
    return (spec.corner_radii.top_left == 0 &&
            spec.corner_radii.top_right == 0 &&
            spec.corner_radii.bottom_left == 0 &&
            spec.corner_radii.bottom_right == 0);
}

inline SubSpecs get_subspecs(Spec spec) {
    const auto origin = wf::origin(spec.geo);

    const auto left_width =
        std::max(spec.corner_radii.top_left, spec.corner_radii.bottom_left);

    const auto right_width =
        std::max(spec.corner_radii.top_right, spec.corner_radii.bottom_right);

    const auto top_height =
        std::max(spec.corner_radii.top_left, spec.corner_radii.top_right);

    const auto bottom_height =
        std::max(spec.corner_radii.bottom_left, spec.corner_radii.bottom_right);

    return {
        // Left side
        wf::geometry_t{
            0,
            spec.corner_radii.top_left,
            left_width,
            spec.geo.height -
                (spec.corner_radii.top_left + spec.corner_radii.bottom_left),
        } + origin,
        // Right side
        wf::geometry_t{
            spec.geo.width - right_width,
            spec.corner_radii.top_right,
            right_width,
            spec.geo.height -
                (spec.corner_radii.top_right + spec.corner_radii.bottom_right),
        } + origin,
        // Top side
        wf::geometry_t{
            spec.corner_radii.top_left,
            0,
            spec.geo.width -
                (spec.corner_radii.top_left + spec.corner_radii.top_right),
            top_height,
        } + origin,
        // Bottom side
        wf::geometry_t{
            spec.corner_radii.bottom_left,
            spec.geo.height - bottom_height,
            spec.geo.width - (spec.corner_radii.bottom_left +
                              spec.corner_radii.bottom_right),
            bottom_height,
        } + origin,

        // Center
        wf::geometry_t{
            left_width,
            top_height,
            spec.geo.width - right_width,
            spec.geo.height - bottom_height,
        } + origin,

        // Top-left corner
        CurveSubSurf::Spec{
            wf::point_t{spec.corner_radii.top_left,
                        spec.corner_radii.top_left} +
                origin,
            M_PI_2,
            M_PI,
            spec.corner_radii.top_left,
            spec.corner_radii.top_left,
        },
        // Top-right corner
        CurveSubSurf::Spec{
            wf::point_t{spec.geo.width - spec.corner_radii.top_right,
                        spec.corner_radii.top_right} +
                origin,
            0,
            M_PI_2,
            spec.corner_radii.top_right,
            spec.corner_radii.top_right,
        },
        // Bottom-left corner
        CurveSubSurf::Spec{
            wf::point_t{spec.corner_radii.bottom_left,
                        spec.geo.height - spec.corner_radii.bottom_left} +
                origin,
            M_PI,
            M_PI + M_PI_2,
            spec.corner_radii.bottom_left,
            spec.corner_radii.bottom_left,
        },
        // Bottom-right corner
        CurveSubSurf::Spec{
            wf::point_t{spec.geo.width - spec.corner_radii.bottom_right,
                        spec.geo.height - spec.corner_radii.bottom_right} +
                origin,
            M_PI + M_PI_2,
            2 * M_PI,
            spec.corner_radii.bottom_right,
            spec.corner_radii.bottom_right,
        },
    };
}

void render(const Spec spec, wf::color_t color, wf::point_t origin,
            glm::mat4 matrix) {
    if (is_rect(spec))
        RectSubSurf::render(spec.geo, color, origin, matrix);

    const auto specs = get_subspecs(spec);
    RectSubSurf::render(specs.left, color, origin, matrix);
    RectSubSurf::render(specs.right, color, origin, matrix);
    RectSubSurf::render(specs.top, color, origin, matrix);
    RectSubSurf::render(specs.bottom, color, origin, matrix);
    RectSubSurf::render(specs.center, color, origin, matrix);

    CurveSubSurf::render(specs.top_left, color, origin, matrix);
    CurveSubSurf::render(specs.top_right, color, origin, matrix);
    CurveSubSurf::render(specs.bottom_left, color, origin, matrix);
    CurveSubSurf::render(specs.bottom_right, color, origin, matrix);
}

wf::region_t calculate_region(const Spec spec) {
    if (is_rect(spec))
        return RectSubSurf::calculate_region(spec.geo);

    wf::region_t region;

    const auto specs = get_subspecs(spec);
    region |= RectSubSurf::calculate_region(specs.left);
    region |= RectSubSurf::calculate_region(specs.right);
    region |= RectSubSurf::calculate_region(specs.top);
    region |= RectSubSurf::calculate_region(specs.bottom);
    region |= RectSubSurf::calculate_region(specs.center);

    region |= CurveSubSurf::calculate_region(specs.top_left);
    region |= CurveSubSurf::calculate_region(specs.top_right);
    region |= CurveSubSurf::calculate_region(specs.bottom_left);
    region |= CurveSubSurf::calculate_region(specs.bottom_right);

    return region;
}

bool contains_point(Spec spec, wf::point_t pt) {
    if (is_rect(spec))
        return RectSubSurf::contains_point(spec.geo, pt);

    const auto specs = get_subspecs(spec);

    return spec.geo & pt ||

           RectSubSurf::contains_point(specs.left, pt) ||
           RectSubSurf::contains_point(specs.right, pt) ||
           RectSubSurf::contains_point(specs.top, pt) ||
           RectSubSurf::contains_point(specs.bottom, pt) ||
           RectSubSurf::contains_point(specs.center, pt) ||

           CurveSubSurf::contains_point(specs.top_left, pt) ||
           CurveSubSurf::contains_point(specs.top_right, pt) ||
           CurveSubSurf::contains_point(specs.bottom_left, pt) ||
           CurveSubSurf::contains_point(specs.bottom_right, pt);
}
} // namespace RoundedRectSubSurf

// BorderSubSurf

namespace BorderSubSurf {
struct SubSpecs {
    const RectSubSurf::Spec left, right, top, bottom;
    const CurveSubSurf::Spec top_left, top_right, bottom_left, bottom_right;
};

inline SubSpecs get_subspecs(Spec spec) {
    return {
        // Left side
        wf::geometry_t{
            0,
            spec.corner_radii.top_left,
            spec.border_width,
            spec.geo.height -
                (spec.corner_radii.top_left + spec.corner_radii.bottom_left),
        },
        // Right side
        wf::geometry_t{
            spec.geo.width - spec.border_width,
            spec.corner_radii.top_right,
            spec.border_width,
            spec.geo.height -
                (spec.corner_radii.top_right + spec.corner_radii.bottom_right),
        },
        // Top side
        wf::geometry_t{
            spec.corner_radii.top_left,
            0,
            spec.geo.width -
                (spec.corner_radii.top_left + spec.corner_radii.top_right),
            spec.border_width,
        },
        // Bottom side
        wf::geometry_t{
            spec.corner_radii.bottom_left,
            spec.geo.height - spec.border_width,
            spec.geo.width - (spec.corner_radii.bottom_left +
                              spec.corner_radii.bottom_right),
            spec.border_width,
        },

        // Top-left corner
        CurveSubSurf::Spec{
            {spec.corner_radii.top_left, spec.corner_radii.top_left},
            M_PI_2,
            M_PI,
            spec.corner_radii.top_left,
            spec.border_width,
        },
        // Top-right corner
        CurveSubSurf::Spec{
            {spec.geo.width - spec.corner_radii.top_right,
             spec.corner_radii.top_right},
            0,
            M_PI_2,
            spec.corner_radii.top_right,
            spec.border_width,
        },
        // Bottom-left corner
        CurveSubSurf::Spec{
            {spec.corner_radii.bottom_left,
             spec.geo.height - spec.corner_radii.bottom_left},
            M_PI,
            M_PI + M_PI_2,
            spec.corner_radii.bottom_left,
            spec.border_width,
        },
        // Bottom-right corner
        CurveSubSurf::Spec{
            {spec.geo.width - spec.corner_radii.bottom_right,
             spec.geo.height - spec.corner_radii.bottom_right},
            M_PI + M_PI_2,
            2 * M_PI,
            spec.corner_radii.bottom_right,
            spec.border_width,
        },
    };
}

void render(const Spec spec, Colors colors, wf::point_t origin,
            glm::mat4 matrix) {
    const auto specs = get_subspecs(spec);
    RectSubSurf::render(specs.left, colors.all, origin, matrix);
    RectSubSurf::render(specs.right, colors.right, origin, matrix);
    RectSubSurf::render(specs.top, colors.all, origin, matrix);
    RectSubSurf::render(specs.bottom, colors.bottom, origin, matrix);

    CurveSubSurf::render(specs.top_left, colors.all, origin, matrix);
    CurveSubSurf::render(specs.top_right, colors.all, origin, matrix);
    CurveSubSurf::render(specs.bottom_left, colors.all, origin, matrix);
    CurveSubSurf::render(specs.bottom_right, colors.all, origin, matrix);
}

wf::region_t calculate_region(const Spec spec) {
    wf::region_t region;

    const auto specs = get_subspecs(spec);
    region |= RectSubSurf::calculate_region(specs.left);
    region |= RectSubSurf::calculate_region(specs.right);
    region |= RectSubSurf::calculate_region(specs.top);
    region |= RectSubSurf::calculate_region(specs.bottom);

    region |= CurveSubSurf::calculate_region(specs.top_left);
    region |= CurveSubSurf::calculate_region(specs.top_right);
    region |= CurveSubSurf::calculate_region(specs.bottom_left);
    region |= CurveSubSurf::calculate_region(specs.bottom_right);

    return region;
}

bool contains_point(Spec spec, wf::point_t pt) {
    const auto specs = get_subspecs(spec);

    return RectSubSurf::contains_point(specs.left, pt) ||
           RectSubSurf::contains_point(specs.right, pt) ||
           RectSubSurf::contains_point(specs.top, pt) ||
           RectSubSurf::contains_point(specs.bottom, pt) ||

           CurveSubSurf::contains_point(specs.top_left, pt) ||
           CurveSubSurf::contains_point(specs.top_right, pt) ||
           CurveSubSurf::contains_point(specs.bottom_left, pt) ||
           CurveSubSurf::contains_point(specs.bottom_right, pt);
}
} // namespace BorderSubSurf

// TitleBarSubSurf

void TitleBarSubSurf::cache_textures(CachedSpec spec) {
    auto size = wf::dimensions(spec.spec.geo);
    size.width -= (int)((double)size.height * 0.2);

    title_text.cache_texture({
        // size
        size,

        // font
        spec.font,

        // text
        spec.title,

        // color
        spec.title_color,
    });
}

auto TitleBarSubSurf::get_subspecs(Spec spec) -> SubSpecs {
    wf::point_t text_pos = wf::origin(spec.geo);
    text_pos.x += (int)((double)spec.geo.height * 0.1);

    return {
        // rounded rect
        {
            spec.geo,
            {
                spec.corner_radii.top_left,
                spec.corner_radii.top_right,
                0,
                0,
            },
        },

        // text
        text_pos,
    };
}

void TitleBarSubSurf::render(Spec spec, wf::color_t color, wf::point_t origin,
                             glm::mat4 matrix) const {
    const auto specs = get_subspecs(spec);
    RoundedRectSubSurf::render(specs.rect, color, origin, matrix);
    title_text.render(specs.text, origin, matrix);
}

[[nodiscard]] wf::region_t TitleBarSubSurf::calculate_region(Spec spec) const {
    wf::region_t region;

    const auto specs = get_subspecs(spec);
    region |= RoundedRectSubSurf::calculate_region(specs.rect);
    region |= title_text.calculate_region(specs.text);

    return region;
}

[[nodiscard]] bool TitleBarSubSurf::contains_point(Spec spec,
                                                   wf::point_t pt) const {
    const auto specs = get_subspecs(spec);

    return RoundedRectSubSurf::contains_point(specs.rect, pt) ||
           title_text.contains_point(specs.text, pt);
}
