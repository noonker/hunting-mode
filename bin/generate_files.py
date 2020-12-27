import importlib
import re
import os

HUNTING_BASEDIR = os.environ['HUNTING_BASE']

modules = os.listdir(HUNTING_BASEDIR + "/src/python/hunting_api/")
modules = [x.replace(".py", "")
           for x in modules
           if x.endswith(".py")
           and "#" not in x]
libs = [importlib.import_module("hunting_api.{}".format(x)) for x in modules]
master = []
[[master.append(y) for y in dir(x)] for x in libs]
functions = [x for x in master if re.search(r'^[a-z]', x)
             and x != "requests"
             and x != "json"
             and x != "datetime"
             and x != "ipaddress"
             and x != "environ"]


org_lib = """
#+name: {1}
#+begin_src python :results drawer value :exports results :var q="", f="", j=""
from hunting_tools.json_table import tablify
from hunting_api.{0} import {1}
function = {1}
if q and f:
    out = function(q, f=f)
elif q:
    out = function(q)
else:
    out = function()
if not j:
    return tablify(out.json())
else:
    return out.json()
#+end_src
"""

helm_string = '(\"{0}\" . \"#+call:~/api:{0}()\")\n'

with open(os.path.expanduser('~') + "/api", "w+") as f:
    for function in functions:
        base = function.split("_")[0]
        f.write(org_lib.format(base, function))

thisfile = os.path.expanduser('~') + "/helm-sources.el"
with open(thisfile, "w+") as f:
    f.write("(setq hunting-mode-api-options '(")
    for function in functions:
        f.write(helm_string.format(function))
    f.write("))")
