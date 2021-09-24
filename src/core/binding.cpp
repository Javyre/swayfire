#include "../nonstd.hpp"
#include "core.hpp"

// Swayfire

bool Swayfire::on_toggle_split_direction(const wf::activator_data_t &) {
    if (auto parent = get_current_workspace()
                          ->get_active_node()
                          ->parent->as_split_node()) {

        if (parent->is_split())
            parent->set_split_type(
                (parent->get_split_type() == SplitType::HSPLIT)
                    ? SplitType::VSPLIT
                    : SplitType::HSPLIT);
        else
            parent->set_split_type(parent->was_vsplit ? SplitType::VSPLIT
                                                      : SplitType::HSPLIT);

        return true;
    }
    return false;
}

bool Swayfire::on_set_tabbed(const wf::activator_data_t &) {
    if (auto parent = get_current_workspace()
                          ->get_active_node()
                          ->parent->as_split_node()) {
        parent->set_split_type(SplitType::TABBED);
        return true;
    }
    return false;
}

bool Swayfire::on_set_stacked(const wf::activator_data_t &) {
    if (auto parent = get_current_workspace()
                          ->get_active_node()
                          ->parent->as_split_node()) {
        parent->set_split_type(SplitType::STACKED);
        return true;
    }
    return false;
}

bool Swayfire::on_set_want_vsplit(const wf::activator_data_t &) {
    if (auto vnode =
            get_current_workspace()->get_active_node()->as_view_node()) {
        vnode->set_prefered_split_type(SplitType::VSPLIT);
        return true;
    }
    return false;
}

bool Swayfire::on_set_want_hsplit(const wf::activator_data_t &) {
    if (auto vnode =
            get_current_workspace()->get_active_node()->as_view_node()) {
        vnode->set_prefered_split_type(SplitType::HSPLIT);
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

bool Swayfire::on_focus_left(const wf::activator_data_t &) {
    return focus_direction(Direction::LEFT);
}
bool Swayfire::on_focus_right(const wf::activator_data_t &) {
    return focus_direction(Direction::RIGHT);
}
bool Swayfire::on_focus_down(const wf::activator_data_t &) {
    return focus_direction(Direction::DOWN);
}
bool Swayfire::on_focus_up(const wf::activator_data_t &) {
    return focus_direction(Direction::UP);
}

bool focus_tiled(WorkspaceRef ws) {
    auto tiled = ws->tiled_root.node->get_last_active_node();
    if (!(tiled.get() == ws->tiled_root.node.get() &&
          ws->tiled_root.node->empty())) {
        tiled->set_active();
        return true;
    }
    return false;
}

bool focus_floating(WorkspaceRef ws) {
    if (auto floating = ws->get_active_floating_node()) {
        if (auto fsplit = floating->as_split_node())
            fsplit->get_last_active_node()->set_active();
        else
            floating->set_active();
        return true;
    }
    return false;
}

bool Swayfire::on_toggle_focus_tile(const wf::activator_data_t &) {
    auto ws = get_current_workspace();
    const bool is_floating =
        ws->get_active_node()->find_floating_parent() != nullptr;

    return is_floating ? focus_tiled(ws) : focus_floating(ws);
}

bool Swayfire::move_direction(Direction dir) {
    return get_current_workspace()->get_active_node()->move(dir);
}

bool Swayfire::on_move_left(const wf::activator_data_t &) {
    return move_direction(Direction::LEFT);
}
bool Swayfire::on_move_right(const wf::activator_data_t &) {
    return move_direction(Direction::RIGHT);
}
bool Swayfire::on_move_down(const wf::activator_data_t &) {
    return move_direction(Direction::DOWN);
}
bool Swayfire::on_move_up(const wf::activator_data_t &) {
    return move_direction(Direction::UP);
}

bool Swayfire::on_toggle_tile(const wf::activator_data_t &) {
    auto ws = get_current_workspace();
    auto const node = ws->get_active_node();

    // If we're floating, we want to tile.
    node->tile_request(node->get_floating());
    return true;
}

void Swayfire::bind_activators() {
    using namespace std::placeholders;

#define BIND_ACTIVATOR(BIND)                                                   \
    {                                                                          \
        auto cb = std::make_unique<wf::activator_callback>(                    \
            [&](auto b) { return on_##BIND(b); });                             \
        output->add_activator(key_##BIND, cb.get());                           \
        activator_callbacks.push_back(std::move(cb));                          \
    }

    BIND_ACTIVATOR(toggle_split_direction);
    BIND_ACTIVATOR(set_tabbed);
    BIND_ACTIVATOR(set_stacked);

    BIND_ACTIVATOR(set_want_vsplit);
    BIND_ACTIVATOR(set_want_hsplit);

    BIND_ACTIVATOR(focus_left);
    BIND_ACTIVATOR(focus_right);
    BIND_ACTIVATOR(focus_down);
    BIND_ACTIVATOR(focus_up);

    BIND_ACTIVATOR(toggle_focus_tile);

    BIND_ACTIVATOR(move_left);
    BIND_ACTIVATOR(move_right);
    BIND_ACTIVATOR(move_down);
    BIND_ACTIVATOR(move_up);

    BIND_ACTIVATOR(toggle_tile);
#undef BIND_ACTIVATOR
}

void Swayfire::unbind_activators() {
    for (auto &cb : activator_callbacks | nonstd::reverse)
        output->rem_binding(cb.get());
    activator_callbacks.clear();
}
