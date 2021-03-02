#include "swayfire.hpp"

// Swayfire

bool Swayfire::on_toggle_split_direction(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);

    if (ws->get_active_node() && ws->get_active_node()->parent) {
        if (auto parent = ws->get_active_node()->parent->as_split_node()) {
            parent->toggle_split_direction();
            return true;
        }
    }
    return false;
}

bool Swayfire::on_set_want_vsplit(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    if (auto vnode = ws->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = SplitType::VSPLIT;
        return true;
    }
    return false;
}

bool Swayfire::on_set_want_hsplit(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    if (auto vnode = ws->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = SplitType::HSPLIT;
        return true;
    }
    return false;
}

bool Swayfire::focus_direction(Direction dir) {
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

bool Swayfire::on_focus_left(wf::keybinding_t) {
    return focus_direction(Direction::LEFT);
}
bool Swayfire::on_focus_right(wf::keybinding_t) {
    return focus_direction(Direction::RIGHT);
}
bool Swayfire::on_focus_down(wf::keybinding_t) {
    return focus_direction(Direction::DOWN);
}
bool Swayfire::on_focus_up(wf::keybinding_t) {
    return focus_direction(Direction::UP);
}

bool Swayfire::on_toggle_focus_tile(wf::keybinding_t) {
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

bool Swayfire::move_direction(Direction dir) {
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

bool Swayfire::on_move_left(wf::keybinding_t) {
    return move_direction(Direction::LEFT);
}
bool Swayfire::on_move_right(wf::keybinding_t) {
    return move_direction(Direction::RIGHT);
}
bool Swayfire::on_move_down(wf::keybinding_t) {
    return move_direction(Direction::DOWN);
}
bool Swayfire::on_move_up(wf::keybinding_t) {
    return move_direction(Direction::UP);
}

bool Swayfire::on_toggle_tile(wf::keybinding_t) {
    auto wsid = output->workspace->get_current_workspace();
    auto ws = workspaces.get(wsid);
    ws->toggle_tile_node(ws->get_active_node());
    return true;
}

void Swayfire::bind_keys() {
    using namespace std::placeholders;

#define ADD_KEY(KEY, BIND)                                                     \
    {                                                                          \
        auto cb = std::make_unique<wf::key_callback>(                          \
            [&](auto b) { return BIND(b); });                                  \
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
#undef ADD_KEY
}

void Swayfire::unbind_keys() {
    std::for_each(key_callbacks.rbegin(), key_callbacks.rend(),
                  [&](auto &cb) { output->rem_binding(cb.get()); });
}
