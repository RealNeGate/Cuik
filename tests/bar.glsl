#version 460 core

layout(location = 0) in vec2 pos;

void main() {
	gl_Position = foo(pos);
}

vec4 foo(in vec2 x) { return x; }

