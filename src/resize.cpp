#include "nonstd.hpp"
#include "swayfire.hpp"

// INode

wf::dimensions_t INode::try_resize(wf::dimensions_t ndims, uint32_t edges) {
    return parent->try_resize_child(this, ndims, edges);
}

// SplitNode

int32_t SplitNode::try_move_front_edge(SplitChildIter child, int32_t delta,
                                       bool use_preferred_sizes) {
    return try_move_edge(child, delta, true, use_preferred_sizes);
}

int32_t SplitNode::try_move_back_edge(SplitChildIter child, int32_t delta,
                                      bool use_preferred_sizes) {
    return try_move_edge(child, delta, false, use_preferred_sizes);
}

int32_t SplitNode::try_move_edge(SplitChildIter child, int32_t delta,
                                 bool front, bool use_prefered_sizes) {
    switch (split_type) {
    case SplitType::TABBED:
    case SplitType::STACKED:
        return 0;
    case SplitType::VSPLIT:
    case SplitType::HSPLIT: {
        if ((front && child < children.begin()) ||
            (!front && child > children.end() - 1)) {
            LOGE("Child out of bounds!");
            return 0;
        }
        break;
    }
    }

    // Wether we are moving an outer edge.
    const bool outer_edge = ((front && child == children.begin()) ||
                             (!front && child == children.end() - 1));

    if (outer_edge) {
        const auto geo = get_geometry();
        const int32_t size =
            (split_type == SplitType::VSPLIT) ? geo.width : geo.height;

        int32_t delta_size = delta * (front ? -1 : 1);

        if (use_prefered_sizes) {
            const int32_t pref_size = (split_type == SplitType::VSPLIT)
                                          ? preferred_size.value().width
                                          : preferred_size.value().height;

            delta_size = std::max(size + delta_size, pref_size) - size;
        }

        delta_size = std::max(size + delta_size,
                              (int32_t)children.size() * MIN_VIEW_SIZE) -
                     size;

        if (delta_size == 0)
            return 0;

        push_safe_set_geo();
        const auto dims =
            (split_type == SplitType::VSPLIT)
                ? wf::dimensions_t({size + delta_size, geo.height})
                : wf::dimensions_t({geo.width, size + delta_size});

        const auto edge = (split_type == SplitType::VSPLIT)
                              ? (front ? WLR_EDGE_LEFT : WLR_EDGE_RIGHT)
                              : (front ? WLR_EDGE_TOP : WLR_EDGE_BOTTOM);

        const int32_t new_size =
            split_type == SplitType::VSPLIT
                ? parent->try_resize_child(this, dims, edge).width
                : parent->try_resize_child(this, dims, edge).height;
        pop_safe_set_geo();

        delta_size = new_size - size;

        {
            const auto distribute_delta_size = [&](auto &c) {
                if (delta_size == 0)
                    return true;

                int32_t old_csize = c.size;
                c.size =
                    (uint32_t)std::max(MIN_VIEW_SIZE, old_csize + delta_size);
                delta_size -= c.size - old_csize;
                return false;
            };

            if (front) {
                for (auto &c : children)
                    if (distribute_delta_size(c))
                        break;
            } else {
                for (auto &c : children | nonstd::reverse)
                    if (distribute_delta_size(c))
                        break;
            }
        }

        refresh_geometry();

        return delta_size * (front ? -1 : 1);
    } else {
        int32_t delta_child_size = delta * (front ? -1 : 1);

        const auto other = child + (front ? -1 : 1);
        const int32_t child_size = child->size;
        const int32_t other_size = other->size;

        if (use_prefered_sizes) {
            const int32_t pref =
                split_type == SplitType::VSPLIT
                    ? other->node->preferred_size.value().width
                    : other->node->preferred_size.value().height;

            delta_child_size =
                other_size - std::min(other_size - delta_child_size, pref);
        }

        delta_child_size =
            other_size - std::max(other_size - delta_child_size, MIN_VIEW_SIZE);

        delta_child_size =
            std::max(child_size + delta_child_size, MIN_VIEW_SIZE) - child_size;

        child->size += delta_child_size;
        other->size -= delta_child_size;

        refresh_geometry();

        return delta_child_size * (front ? -1 : 1);
    }
}

wf::dimensions_t SplitNode::try_resize_child(Node node, wf::dimensions_t ndims,
                                             uint32_t edges) {
    // Forward iterator starting at child.
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);
    // Reverse iterator starting at child.
    auto rchild = std::reverse_iterator(child + 1);

    const auto child_geo = child->node->get_geometry();

    ndims = nonwf::max(ndims, {MIN_VIEW_SIZE, MIN_VIEW_SIZE});

    wf::dimensions_t delta = {
        ndims.width - child_geo.width,
        ndims.height - child_geo.height,
    };

    if ((edges & (WLR_EDGE_LEFT | WLR_EDGE_RIGHT)) == 0)
        delta.width = 0;
    if ((edges & (WLR_EDGE_TOP | WLR_EDGE_BOTTOM)) == 0)
        delta.height = 0;

    if (delta.width == 0 && delta.height == 0)
        return wf::dimensions(child_geo);

    switch (split_type) {
    case SplitType::VSPLIT:
    case SplitType::HSPLIT: {
        push_safe_set_geo();

        auto delta_size =
            split_type == SplitType::VSPLIT ? delta.width : delta.height;

        const bool front_edge =
            (split_type == SplitType::VSPLIT && (edges & WLR_EDGE_LEFT)) ||
            (split_type == SplitType::HSPLIT && (edges & WLR_EDGE_TOP));

        if (front_edge) {
            // Shrinking child from left means moving left edge(s) towards +x.
            auto edge_delta = delta_size * -1;
            if (delta_size > 0) {
                // Shrink siblings starting with nearest
                for (; rchild != children.rend() && edge_delta < 0; rchild++)
                    edge_delta -=
                        try_move_front_edge((rchild + 1).base(), edge_delta);

            } else {
                // Grow siblings starting with furthest
                auto front = children.begin();
                for (; front != child && edge_delta > 0; front++)
                    edge_delta -= try_move_front_edge(front, edge_delta, true);

                if (front == child && edge_delta > 0)
                    edge_delta -= try_move_front_edge(front, edge_delta);
            }
            delta_size = edge_delta * -1;
        } else {
            auto edge_delta = delta_size;
            if (delta_size > 0) {
                // Shrink siblings starting with nearest
                for (; child != children.end() && edge_delta > 0; child++)
                    edge_delta -= try_move_back_edge(child, edge_delta);
            } else {
                // Grow siblings starting with furthest
                auto back = children.rbegin();
                for (; back != rchild && edge_delta < 0; back++)
                    edge_delta -=
                        try_move_back_edge((back + 1).base(), edge_delta, true);

                if (back == rchild && edge_delta < 0)
                    edge_delta -=
                        try_move_back_edge((back + 1).base(), edge_delta);
            }
            delta_size = edge_delta;
        }

        auto other_dim =
            (split_type == SplitType::VSPLIT) ? delta.height : delta.width;
        if (other_dim != 0) {
            wf::dimensions_t nndims = wf::dimensions(get_geometry());
            if (split_type == SplitType::VSPLIT)
                nndims.height += delta.height;
            else
                nndims.width += delta.width;

            parent->try_resize_child(this, nndims, edges);
        }

        pop_safe_set_geo();
        refresh_geometry();

        return wf::dimensions(node->get_geometry());
    }
    case SplitType::TABBED:
    case SplitType::STACKED: {
        parent->try_resize_child(this, ndims, edges);
        return wf::dimensions(node->get_geometry());
    }
    }
}

// Workspace

wf::dimensions_t Workspace::try_resize_child(Node child, wf::dimensions_t ndims,
                                             uint32_t edges) {
    if (child->get_floating()) {
        auto ngeo = child->get_geometry();

        auto hori_locked = (edges & (WLR_EDGE_LEFT | WLR_EDGE_RIGHT)) == 0;
        auto vert_locked = (edges & (WLR_EDGE_TOP | WLR_EDGE_BOTTOM)) == 0;

        if (!hori_locked) {
            if (edges & WLR_EDGE_LEFT) {
                int dw = ndims.width - ngeo.width;
                ngeo.x -= dw;
            }
            ngeo.width = ndims.width;
        }

        if (!vert_locked) {
            if (edges & WLR_EDGE_TOP) {
                int dh = ndims.height - ngeo.height;
                ngeo.y -= dh;
            }
            ngeo.height = ndims.height;
        }

        child->set_geometry(ngeo);
        return wf::dimensions(ngeo);
    }
    return wf::dimensions(child->get_geometry());
}
void SplitNode::begin_resize() {
    INode::begin_resize();

    sync_sizes_to_ratios();

    for (auto &child : children)
        child.node->begin_resize();
}

void SplitNode::end_resize() {
    INode::end_resize();

    sync_ratios_to_sizes();

    for (auto &child : children)
        child.node->end_resize();
}
