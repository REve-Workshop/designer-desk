
from ansible.errors import AnsibleFilterError

def actionsplice(global_actions, local_actions):
    """Return the combination of 2 instances of an action-like dictionary.

An action-like dictionary can have any combination of the following key/value pair fields:
- config
- xdg_compliant
- checkout
- delete_xdgdir_first

Other fields are accepted in local_actions. Any extra field in global_actions will be ignored

Values in fields of local_actions have precedence over values in fields of global_actions.

The actions checked in this function have nothing to do with ansible actions. I should surely find another word for them.
"""

    out_actions = {}
    actions_list = ['config', 'xdg_compliant', 'checkout', 'delete_xdgdir_first']
    for key, value in local_actions.items():
        out_actions[key] = value
        try:
            actions_list.remove(key)
        except ValueError:
            pass

    for act in actions_list:
        if act in global_actions:
            out_actions[act] = global_actions[act]
        else:
            out_actions[act] = False

    return out_actions

class FilterModule(object):
    ''' Reve Workshop filters '''

    def filters(self):
        return {'actionsplice': actionsplice}
