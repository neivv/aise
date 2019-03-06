#version 130

in vec2 position;
in vec4 color;
out vec4 frag_color;
out vec2 frag_position;

uniform mat4 transform;

void main() {
    vec4 transformed = transform * vec4(position, 0.0, 1.0);
    gl_Position = transformed;
    frag_color = color;
    frag_position = transformed.xy;
}
