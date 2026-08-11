import gi
gi.require_version('Atspi', '2.0')
from gi.repository import Atspi
Atspi.init()
d = Atspi.get_desktop(0)
print(f"APPS={d.get_child_count()}")
for i in range(d.get_child_count()):
    a = d.get_child_at_index(i)
    n = a.get_name()
    r = a.get_role_name()
    if 'jasp' not in n.lower(): continue
    cc = a.get_child_count()
    print(f"JASP[{i}] children={cc}")
    for j in range(min(cc, 5)):
        try:
            ch = a.get_child_at_index(j)
            cn = ch.get_name()
            cr = ch.get_role_name()
            cc2 = ch.get_child_count()
            print(f"  [{j}] {cn!r} role={cr} cc={cc2}")
            for k in range(min(cc2, 4)):
                try:
                    gc = ch.get_child_at_index(k)
                    print(f"    [{k}] {gc.get_name()!r} role={gc.get_role_name()}")
                except: pass
        except: pass
