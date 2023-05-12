// #version 310 es

layout(location = 0) in vec2 vPosition;

#if 0
layout(location = 1) in vec2 vUV;
layout(location = 2) in vec4 vColor;
layout(location = 3, baz = 1) in int vTex;

out vec2 uv;
out vec4 color;
flat out int tex;

layout(location = 0) uniform vec2 uViewport;

void main() {
    uv = vUV;
    color = vColor.bgra;
    tex = vTex;
    gl_Position = vec4((vPosition * uViewport) - 1.0, 0.0, 1.0);
}
#endif
