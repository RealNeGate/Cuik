local xmlparser = require "meta/xmlparser"
local inspect = require "meta/inspect"

doc, err = xmlparser.parseFile("Y:/Workspace/instructions.xml", false)

for k,v in pairs(doc.children[1].children) do
    -- find extensions we care about
    print(v.attrs.name)
    if v.attrs.name == "BASE" then
        for k2,v2 in pairs(v.children) do
            local arch = nil
            for k3,v3 in pairs(v2.children) do
                if v3.tag == "architecture" and v3.attrs.name == "SKL" then
                    arch = v3
                    break
                end
            end

            if arch then
                print(">", v2.attrs.string, arch.attrs.name, arch.children[1].attrs.TP_ports, arch.children[1].attrs.ports)
            end
        end
    end
end

