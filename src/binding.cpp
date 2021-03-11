#include "swayfire.hpp"

// Swayfire

bool Swayfire::on_toggle_split_direction(wf::keybinding_t) {
    auto ws = get_current_workspace();

    if (ws->get_active_node()) {
        if (auto parent = ws->get_active_node()->parent->as_split_node()) {
            parent->toggle_split_direction();
            return true;
        }
    }
    return false;
}

bool Swayfire::on_set_want_vsplit(wf::keybinding_t) {
    if (auto vnode =
            get_current_workspace()->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = SplitType::VSPLIT;
        return true;
    }
    return false;
}

bool Swayfire::on_set_want_hsplit(wf::keybinding_t) {
    if (auto vnode =
            get_current_workspace()->get_active_node()->as_view_node()) {
        vnode->prefered_split_type = SplitType::HSPLIT;
        return true;
    }
    return false;
}

bool Swayfire::focus_direction(Direction dir) {
    auto active = get_current_workspace()->get_active_node();
    if (auto adj = active->parent->get_adjacent(active, dir)) {
        if (auto split = adj->as_split_node())
            adj = split->get_last_active_node();

        adj->set_active();
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
    auto ws = get_current_workspace();
    if (ws->get_active_node()->get_floating()) {
        if (auto tiled = ws->get_active_tiled_node())
            tiled->set_active();
        else
            return false;
    } else {
        if (auto floating = ws->get_active_floating_node())
            floating->set_active();
        else
            return false;
    }
    return true;
}

bool Swayfire::move_direction(Direction dir) {
    auto ws = get_current_workspace();
    auto active = ws->get_active_node();

    auto old_parent = active->parent;
    if (!active->parent->move_child(active, dir))
        return false;

    if (old_parent.get() != ws->tiled_root.get()) {
        if (auto old_parent_split = old_parent->as_split_node()) {
            old_parent_split->try_downgrade();
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
    auto ws = get_current_workspace();
    ws->toggle_tile_node(ws->get_active_node());
    return true;
}

void Swayfire::bind_keys() {
    using namespace std::placeholders;

#define BIND_KEY(BIND)                                                         \
    {                                                                          \
        auto cb = std::make_unique<wf::key_callback>(                          \
            [&](auto b) { return on_##BIND(b); });                             \
        output->add_key(key_##BIND, cb.get());                                 \
        key_callbacks.push_back(std::move(cb));                                \
    }

    BIND_KEY(toggle_split_direction);

    BIND_KEY(set_want_vsplit);
    BIND_KEY(set_want_hsplit);

    BIND_KEY(focus_left);
    BIND_KEY(focus_right);
    BIND_KEY(focus_down);
    BIND_KEY(focus_up);

    BIND_KEY(toggle_focus_tile);

    BIND_KEY(move_left);
    BIND_KEY(move_right);
    BIND_KEY(move_down);
    BIND_KEY(move_up);

    BIND_KEY(toggle_tile);
#undef ADD_KEY
}

void Swayfire::unbind_keys() {
    std::for_each(key_callbacks.rbegin(), key_callbacks.rend(),
                  [&](auto &cb) { output->rem_binding(cb.get()); });
}
