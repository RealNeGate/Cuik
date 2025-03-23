local xmlparser = require "meta/xmlparser"
local inspect = require "meta/inspect"

doc, err = xmlparser.parseFile("Y:/Workspace/instructions.xml", false)

local insts = {}

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
                local a, b = v2.attrs.string:match"^(%S+)%s+%((.+)%)"
                if a and b then
                    -- some instruction names have weird shit like the exact opcode
                    -- written after it, trim them off
                    local c = a:match"(.+)_.*"
                    if c then
                        a = c
                    end

                    if not insts[a] then
                        insts[a] = {}
                    end

                    local max_lat = 1
                    for k3,v3 in pairs(arch.children[1].children) do
                        for k4,v4 in pairs(v3.attrs) do
                            if k4 ~= "start_op" and k4 ~= "target_op" then
                                local lat = tonumber(v4)
                                if lat and lat > max_lat then
                                    max_lat = lat
                                end
                            end
                        end
                    end

                    if insts[a][b] then
                        print("Duplicate info on ", a, b)
                        if max_lat > insts[a][b].lat then
                            insts[a][b].lat = max_lat
                        end
                    else
                        insts[a][b] = { tp = arch.children[1].attrs.TP_ports, ports = arch.children[1].attrs.ports, lat = max_lat }
                    end

                    -- print(">", a, "|", "("..b..")", arch.attrs.name, arch.children[1].attrs.TP_ports, arch.children[1].attrs.ports)
                else
                    -- print(">", v2.attrs.string, arch.attrs.name, arch.children[1].attrs.TP_ports, arch.children[1].attrs.ports)
                end
            end
        end
    end
end

function compile_inst(inst)

end

compile_inst(insts["ADD"])

print(inspect(insts.ADD))

