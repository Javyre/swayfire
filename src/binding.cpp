#include "swayfire.hpp"

// swayfire_t

void workspace_t::toggle_split_direction_node(node_t node) {
    if (auto parent = node->parent->as_split_node()) {
        LOGD("Toggling split dir: ", parent);

        parent->split_type = (parent->split_type == split_type_t::HSPLIT)
                                 ? split_type_t::VSPLIT
                                 : split_type_t::HSPLIT;

        parent->refresh_geometry();
    }
}

bool swayfire_t::on_toggle_split_direction(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);

    if (ws->get_active_node() && ws->get_active_node()->parent) {
        ws->toggle_split_direction_node(ws->get_active_node());
        return true;
    }
    return false;
}

bool swayfire_t::on_set_want_vsplit(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    if (auto vnode = ws->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = split_type_t::VSPLIT;
        return true;
    }
    return false;
}

bool swayfire_t::on_set_want_hsplit(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    if (auto vnode = ws->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = split_type_t::HSPLIT;
        return true;
    }
    return false;
}

bool swayfire_t::focus_direction(direction_t dir) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    auto active = ws->get_active_node();
    if (auto adj = active->parent->get_adjacent(active, dir)) {
        if (auto split = adj->as_split_node())
            adj = split->get_last_active_node();

        ws->set_active_node(adj);
        return true;
    }
    return false;
}

bool swayfire_t::on_focus_left(wf::keybinding_t) {
    return focus_direction(direction_t::LEFT);
}
bool swayfire_t::on_focus_right(wf::keybinding_t) {
    return focus_direction(direction_t::RIGHT);
}
bool swayfire_t::on_focus_down(wf::keybinding_t) {
    return focus_direction(direction_t::DOWN);
}
bool swayfire_t::on_focus_up(wf::keybinding_t) {
    return focus_direction(direction_t::UP);
}

bool swayfire_t::on_toggle_focus_tile(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    if (ws->get_active_node()->get_floating()) {
        if (auto tiled = ws->get_active_tiled_node())
            ws->set_active_node(tiled);
        else
            return false;
    } else {
        if (auto floating = ws->get_active_floating_node())
            ws->set_active_node(floating);
        else
            return false;
    }
    return true;
}

bool swayfire_t::move_direction(direction_t dir) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    auto active = ws->get_active_node();

    auto old_parent = active->parent;
    if (!active->parent->move_child(active, dir))
        return false;

    if (old_parent.get() != ws->tiled_root.get()) {
        if (auto parent_split = old_parent->as_split_node()) {
            if (parent_split->children.size() == 1) {
                auto only_child = parent_split->remove_child(
                    parent_split->get_last_active_node());

                if (auto vnode = only_child->as_view_node())
                    vnode->prefered_split_type = parent_split->split_type;

                parent_split->parent->swap_child(parent_split,
                                                 std::move(only_child));
            }
        }
    }

    ws->set_active_child(active);
    return true;
}

bool swayfire_t::on_move_left(wf::keybinding_t) {
    return move_direction(direction_t::LEFT);
}
bool swayfire_t::on_move_right(wf::keybinding_t) {
    return move_direction(direction_t::RIGHT);
}
bool swayfire_t::on_move_down(wf::keybinding_t) {
    return move_direction(direction_t::DOWN);
}
bool swayfire_t::on_move_up(wf::keybinding_t) {
    return move_direction(direction_t::UP);
}

bool swayfire_t::on_toggle_tile(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    ws->toggle_tile_node(ws->get_active_node());
    return true;
}

void swayfire_t::bind_keys() {
    using namespace std::placeholders;

#define ADD_KEY(KEY, BIND)                                                     \
    {                                                                          \
        auto mem_fn = std::bind(std::mem_fn(&swayfire_t::BIND), this, _1);     \
        auto cb = std::make_unique<wf::key_callback>(mem_fn);                  \
        output->add_key(KEY, cb.get());                                        \
        key_callbacks.push_back(std::move(cb));                                \
    }

    ADD_KEY(key_toggle_split_direction, on_toggle_split_direction);

    ADD_KEY(key_set_want_vsplit, on_set_want_vsplit);
    ADD_KEY(key_set_want_hsplit, on_set_want_hsplit);

    ADD_KEY(key_focus_left, on_focus_left);
    ADD_KEY(key_focus_right, on_focus_right);
    ADD_KEY(key_focus_down, on_focus_down);
    ADD_KEY(key_focus_up, on_focus_up);

    ADD_KEY(key_toggle_focus_tile, on_toggle_focus_tile);

    ADD_KEY(key_move_left, on_move_left);
    ADD_KEY(key_move_right, on_move_right);
    ADD_KEY(key_move_down, on_move_down);
    ADD_KEY(key_move_up, on_move_up);

    ADD_KEY(key_toggle_tile, on_toggle_tile);
}

void swayfire_t::unbind_keys() {
    std::for_each(key_callbacks.rbegin(), key_callbacks.rend(),
                  [&](auto &cb) { output->rem_binding(cb.get()); });
}
