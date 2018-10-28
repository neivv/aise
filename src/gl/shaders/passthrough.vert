#version 130

in vec2 position;
out vec2 pos;

uniform mat4 transform;

void main() {
    vec4 transformed = transform * vec4(position, 0.0, 1.0);
    gl_Position = transformed;
    pos = transformed.xy;
}
