#ifndef SWAYFIRE_PLUGIN_HPP
#define SWAYFIRE_PLUGIN_HPP

#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/object.hpp>
#include <wayfire/output.hpp>
#include <wayfire/plugin.hpp>

class Swayfire;
using SwayfireRef = nonstd::observer_ptr<Swayfire>;

/// Reference to swayfire stored in the output.
struct SwayfireCustomData : public wf::custom_data_t {
    SwayfireRef swayfire;
    SwayfireCustomData(SwayfireRef swayfire) : swayfire(swayfire) {}
};

/// Utilities for swayfire plugins loaded through swayfire.
class SwayfirePlugin : public wf::plugin_interface_t {
  private:
    wf::signal_connection_t on_swayfire_init = [&](wf::signal_data_t *) {
        swayfire =
            output->get_data<SwayfireCustomData>("swayfire-core")->swayfire;
        swf_init();
    };

    wf::signal_connection_t on_swayfire_fini = [&](wf::signal_data_t *) {
        swf_fini();
    };

  public:
    /// Pointer to the active Swayfire plugin on this output.
    SwayfireRef swayfire = nullptr;

    /// Run plugin initialization. This is guaranteed to run after swayfire
    /// core's init.
    virtual void swf_init() = 0;

    /// Shut down plugin. This runs right before swayfire core's fini.
    virtual void swf_fini(){};

    // == Impl wf::plugin_interface_t ==
    void init() final {
        if (output->get_data<SwayfireCustomData>("swayfire-core"))
            on_swayfire_init.emit(nullptr);
        else
            output->connect_signal("swf-init", &on_swayfire_init);

        output->connect_signal("swf-fini", &on_swayfire_fini);
    }
    void fini() final {}
};

#endif // SWAYFIRE_PLUGIN_HPP
