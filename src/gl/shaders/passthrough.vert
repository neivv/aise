#version 130

in vec2 position;
out vec2 pos;

void main() {
    gl_Position = vec4(position, 0.0, 1.0);
    pos = position;
}
