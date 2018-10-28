#version 130

in vec2 position;
in vec2 tex_coord;
out vec2 frag_texcoord;

uniform mat4 pos_transform;
uniform mat3 tex_transform;

void main() {
    vec4 transformed = pos_transform * vec4(position, 0.0, 1.0);
    gl_Position = transformed;
    frag_texcoord = (tex_transform * vec3(tex_coord.xy, 1.0)).xy;
}
