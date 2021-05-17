#include "subsurf.hpp"

#include <wayfire/opengl.hpp>

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

void RectSubSurf::render(wf::point_t origin, glm::mat4 matrix) const {
    OpenGL::render_rectangle(get_geo() + origin, get_color(), matrix);
}

wf::region_t RectSubSurf::calculate_region() const { return get_geo(); }

bool RectSubSurf::contains_point(wf::point_t pt) const {
    return wf::region_t{get_geo()}.contains_point(pt);
}

// CurveSubSurf

void CurveSubSurf::render(wf::point_t origin, glm::mat4 matrix) const {
    const auto spec = get_spec();
    const auto color = get_color();
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

wf::region_t CurveSubSurf::calculate_region() const {
    const auto spec = get_spec();
    return wf::geometry_t{
        spec.origin.x - spec.radius,
        spec.origin.y - spec.radius,
        2 * spec.radius,
        2 * spec.radius,
    };
}

bool CurveSubSurf::contains_point(wf::point_t pt) const {
    const auto spec = get_spec();
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
