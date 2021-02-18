#include "swayfire.hpp"
#include "grab.hpp"

#include <bits/stdint-uintn.h>
#include <memory>
#include <wayfire/config/types.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/plugin.hpp>

// active_grab_t

std::unique_ptr<active_grab_t> active_grab_t::try_activate(
        nonstd::observer_ptr<swayfire_t> plugin,
        std::function<std::unique_ptr<active_grab_t>()> cons) {

    if (plugin->output->activate_plugin(plugin->grab_interface)) {
        if (plugin->grab_interface->grab()) {
            return cons();
        } else {
            plugin->output->deactivate_plugin(plugin->grab_interface);
        }
    }

    return nullptr;
}

active_grab_t::~active_grab_t() {
    plugin->output->deactivate_plugin(plugin->grab_interface);
}

// active_button_drag_t

void active_button_drag_t::button(uint32_t b, uint32_t state) {
    if (b == deactivate_button && state == WLR_BUTTON_RELEASED) {
        plugin->active_grab = nullptr;
    }
}

// active_move_t

void active_move_t::pointer_motion(uint32_t x, uint32_t y) {
    auto geo = original_geo;
    geo.x += x - pointer_start.x;
    geo.y += y - pointer_start.y;
    dragged->set_geometry(geo);
}

std::unique_ptr<active_grab_t> active_move_t::construct(
        nonstd::observer_ptr<swayfire_t> plugin,
        node_t dragged) {

    return try_activate(plugin, [&]() {
        auto ret = std::make_unique<active_move_t>(
                plugin, plugin->button_move_activate);
        auto p = wf::get_core().get_cursor_position();

        ret->dragged = dragged;
        ret->original_geo = dragged->get_geometry();
        ret->pointer_start = {(int)p.x, (int)p.y};

        return ret;
    });
}

// active_resize_t

void active_resize_t::pointer_motion(uint32_t x, uint32_t y) {
    // TODO: impl drag resizing
}

std::unique_ptr<active_grab_t> active_resize_t::construct(
        nonstd::observer_ptr<swayfire_t> plugin,
        node_t dragged) {
    // TODO: impl drag resizing
    return nullptr;
}

// swayfire_t

void swayfire_t::init_grab_interface() {
    grab_interface->name = "swayfire";
    grab_interface->capabilities =
        wf::CAPABILITY_GRAB_INPUT | wf::CAPABILITY_MANAGE_DESKTOP;


    grab_interface->callbacks.pointer.motion = [&](uint32_t x, uint32_t y) {
        if (active_grab)
            active_grab->pointer_motion(x, y);
    };

    grab_interface->callbacks.pointer.button = [&](uint32_t b, uint32_t state) {
        if (active_grab)
            active_grab->button(b, state);
    };

    grab_interface->callbacks.touch.motion = [&](int32_t id, int32_t x, int32_t y) {
        if (active_grab && id == 1)
            active_grab->pointer_motion(x, y);
    };

    on_move_activate = [&](auto){
        if (auto view = wf::get_core().get_cursor_focus_view()) {
            if (auto vdata = view->get_data<view_data_t>()) {
                if (auto node = vdata->node->find_floating_parent()) {
                    if (auto active = active_move_t::construct(this, node)) {
                        active_grab = std::move(active);
                        return true;
                    }
                }
            }
        }
        return false;
    };

    on_resize_activate = [&](auto){
        // TODO: impl this
        return false;
    };

    output->add_button(button_move_activate, &on_move_activate);
    output->add_button(button_resize_activate, &on_resize_activate);
}

void swayfire_t::fini_grab_interface() {
    output->rem_binding(&on_resize_activate);
    output->rem_binding(&on_move_activate);

    active_grab = nullptr;
}
