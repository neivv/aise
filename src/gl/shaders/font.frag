#version 130

out vec4 color;

in vec2 frag_texcoord;

uniform sampler2D glyphs;

void main() {
    float alpha = texture(glyphs, frag_texcoord).x;
    color = vec4(0.0, 0.0, 0.0, alpha);
}
