#ifndef SWAYFIRE_HPP
#define SWAYFIRE_HPP

#include <bits/stdint-intn.h>
#include <memory>
#include <sys/types.h>
#include <vector>
#include <variant>

#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/nonstd/noncopyable.hpp>
#include <wayfire/object.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/output.hpp>
#include <wayfire/util/log.hpp>
#include <wayfire/workspace-manager.hpp>
#include <wayfire/signal-definitions.hpp>


enum struct split_type_t : uint8_t {
    VSPLIT, HSPLIT, TABBED, STACKED,
};

class node_interface_t;
class split_node_t;
class view_node_t;

using owned_node_t = std::unique_ptr<node_interface_t>;
using node_t = nonstd::observer_ptr<node_interface_t>;

using split_node_ref_t = nonstd::observer_ptr<split_node_t>;
using view_node_ref_t = nonstd::observer_ptr<view_node_t>;

class node_parent_interface_t {
    public:
        split_node_ref_t as_split_node();

        // virtual wf::geometry_t get_geometry() = 0;
        virtual void insert_child(owned_node_t node) = 0;
        virtual owned_node_t remove_child(node_t node) = 0;
        virtual owned_node_t swap_child(node_t node, owned_node_t other) = 0;

        virtual std::string to_string() const = 0;

        virtual ~node_parent_interface_t() = default;
};

using node_parent_t = nonstd::observer_ptr<node_parent_interface_t>;

static uint id_counter;

class node_interface_t {
    protected:
        bool floating = false;
        wf::point_t wsid = {0, 0};
        wf::geometry_t geometry;
        uint node_id;

        node_interface_t() : node_id(id_counter) { id_counter++; }

    public:
        /* split_node_ref_t parent; */
        node_parent_t parent;

        virtual wf::geometry_t get_geometry() { return geometry; }
        virtual void set_geometry(wf::geometry_t geo) = 0;
        void refresh_geometry() { set_geometry(get_geometry()); }

        bool get_floating() { return floating; };
        virtual void set_floating(bool fl) = 0;

        wf::point_t get_wsid() { return wsid; };
        virtual void set_wsid(wf::point_t wsid) = 0;

        /* virtual node_t deepest_active_node() = 0; */
        virtual node_parent_t get_active_parent_node() = 0;

        virtual ~node_interface_t() = default;

        virtual std::string to_string() const = 0;
};

class view_node_t : public node_interface_t {
    public:
        wayfire_view view;
        std::optional<split_type_t> prefered_split_type;

        view_node_t(wayfire_view view) : view(view) {}

        virtual void set_geometry(wf::geometry_t geo);
        virtual void set_floating(bool fl);
        virtual void set_wsid(wf::point_t wsid);
        /* virtual node_t deepest_active_node(); */
        virtual node_parent_t get_active_parent_node();

        virtual std::string to_string() const {
            std::ostringstream out;
            out << "view-node-" << node_id;
            return out.str(); 
        };
};

struct view_data_t : wf::custom_data_t {
    view_node_ref_t node;

    view_data_t(view_node_ref_t node) : node(node) {}
};

class split_node_t : public node_interface_t, public node_parent_interface_t {
    public:
        split_type_t split_type = split_type_t::VSPLIT;
        uint32_t active_child = 0;
        std::vector<float> children_ratios;
        std::vector<owned_node_t> children;

        split_node_t(wf::geometry_t geo) { geometry = geo; }

        void insert_child_front(owned_node_t node);
        void insert_child_back(owned_node_t node);
        virtual void insert_child(owned_node_t node) { insert_child_back(std::move(node)); };

        virtual owned_node_t remove_child(node_t node);

        virtual void set_geometry(wf::geometry_t geo);
        virtual void set_floating(bool fl);
        virtual void set_wsid(wf::point_t wsid);
        /* virtual node_t deepest_active_node(); */
        virtual node_parent_t get_active_parent_node();
        virtual owned_node_t swap_child(node_t node, owned_node_t other);

        virtual std::string to_string() const {
            std::ostringstream out;
            out << "split-node-" << node_id;
            return out.str(); 
        };

};

struct workspace_t : public node_parent_interface_t {
    wf::point_t wsid;
    wf::geometry_t geometry;
    std::unique_ptr<split_node_t> tiled_root;
    std::vector<owned_node_t> floating_nodes;
    node_t active_node;

    workspace_t(wf::point_t wsid, wf::geometry_t geo) :
        wsid(wsid),
        geometry(geo),
        tiled_root(std::make_unique<split_node_t>(geo)),
        active_node(tiled_root) {};

    workspace_t(workspace_t &) = delete;
    workspace_t(workspace_t &&) = default;
    workspace_t &operator=(workspace_t &&) = default;

    node_parent_t get_active_parent_node();


    // Floating nodes are always direct children of the workspace
    void insert_floating_node(owned_node_t node);
    owned_node_t remove_floating_node(node_t node);
    owned_node_t swap_floating_node(node_t node, owned_node_t other);

    // These methods will find the node in the workspace
    void insert_tiled_node(owned_node_t node);
    owned_node_t remove_tiled_node(node_t node);
    owned_node_t remove_node(node_t node);
    void toggle_tile_node(node_t node);
    void toggle_split_direction_node(node_t node);

    // These methods work with only direct children of the workspace
    // They will not walk the tree like other methods would
    virtual void insert_child(owned_node_t node);
    virtual owned_node_t remove_child(node_t node);
    virtual owned_node_t swap_child(node_t node, owned_node_t other);

    void set_workarea(wf::geometry_t geo);
    virtual wf::geometry_t get_geometry() { return geometry; }

    virtual std::string to_string() const {
        std::ostringstream out;
        out << "workspace-" << wsid;
        return out.str(); 
    };
};

struct workspaces_t {
    // Workspace tree roots: workspaces[x][y]
    std::vector<std::vector<workspace_t>> workspaces;

    void update_dims(wf::dimensions_t ndims, wf::geometry_t geo);

    workspace_t &get(wf::point_t ws);

    void for_each(std::function<void(workspace_t &)> fun);
};

class swayfire_workspace_implementation_t : public wf::workspace_implementation_t {
    public:
        bool view_movable(wayfire_view view) override {
            if (auto vdata = view->get_data<view_data_t>())
                return vdata->node->get_floating();

            return false;
        }

        bool view_resizable(wayfire_view view) override {
            if (auto vdata = view->get_data<view_data_t>())
                return vdata->node->get_floating();

            return false;
        }
};

std::unique_ptr<view_node_t> init_view_node(wayfire_view view);

inline bool is_shutting_down() {
    return wf::get_core().get_current_state() == wf::compositor_state_t::SHUTDOWN;
}

class swayfire_t : public wf::plugin_interface_t {
    private:
        workspaces_t workspaces;

        wf::option_wrapper_t<wf::keybinding_t>
            key_toggle_tile{"swayfire/key_toggle_tile"};

        wf::option_wrapper_t<wf::keybinding_t> 
            key_toggle_split_direction{"swayfire/key_toggle_split_direction"};

        wf::key_callback on_toggle_tile = [&](auto){
            auto wsid = output->workspace->get_current_workspace();
            auto &ws = workspaces.get(wsid);
            ws.toggle_tile_node(ws.active_node);
            return true;
        };

        wf::key_callback on_toggle_split_direction = [&](auto){
            auto wsid = output->workspace->get_current_workspace();
            auto &ws = workspaces.get(wsid);

            if (ws.active_node && ws.active_node->parent) {
                ws.toggle_split_direction_node(ws.active_node);
                return true;
            }
            return false;
        };

        wf::signal_connection_t on_shutdown = [&](wf::signal_data_t *) {
            output->disconnect_signal(&on_view_unmapped);
        };

        wf::signal_connection_t on_view_attached = [&](wf::signal_data_t *data) {
            auto view = wf::get_signaled_view(data);

            if (view->role != wf::VIEW_ROLE_TOPLEVEL)
                return;

            auto wsid = output->workspace->get_current_workspace();
            auto &ws = workspaces.get(wsid);

            LOGD("attaching node in ws: ", wsid, ", ", view->to_string(),
                    " : ", view->get_title());

            ws.insert_tiled_node(init_view_node(view));
        };

        wf::signal_connection_t on_view_unmapped = [&](wf::signal_data_t *data) {
            fini_view(wf::get_signaled_view(data));
        };

        wf::signal_connection_t on_view_focused = [&](wf::signal_data_t *data) {
            auto view = wf::get_signaled_view(data);
            if (auto vdata = view->get_data<view_data_t>()) {
                auto node = vdata->node;

                workspaces.get(node->get_wsid()).active_node = node;
            }
        };

        wf::signal_connection_t on_workarea_changed = [&](wf::signal_data_t *data) {
            auto wcdata = static_cast<wf::workarea_changed_signal *>(data);
            workspaces.for_each([&](auto &ws) {
                ws.set_workarea(wcdata->new_workarea);
            });
        };

        void fini_view(wayfire_view view);

        void bind_signals();
        void unbind_signals();

        void bind_keys();
        void unbind_keys();

    public:
        void init() override;
        void fini() override;
};

#define TO_STRING_OSTREAM(REF)                                 \
    std::ostream& operator<<(std::ostream& os, const REF& n) { \
        os << (n ? n->to_string() : "(null)");                 \
        return os;                                             \
    }

TO_STRING_OSTREAM(node_t);
TO_STRING_OSTREAM(node_parent_t);
TO_STRING_OSTREAM(split_node_ref_t);
TO_STRING_OSTREAM(view_node_ref_t);

#undef TO_STRING_OSTREAM

#endif // ifndef SWAYFIRE_HPP
