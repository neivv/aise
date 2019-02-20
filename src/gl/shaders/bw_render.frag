#version 130

uniform usampler2D image;
uniform sampler1D palette;

in vec2 pos;
out vec4 color;

mat3 conversion = mat3(
    0.5,  0.0, 0.0,
    0.0, -0.5, 0.0,
    0.5,  0.5, 1.0
);

void main() {
    vec3 texcoord = conversion * vec3(pos, 1.0);
    float pal_id = float(texture(image, texcoord.xy).x);
    vec4 pixel_color = texture(palette, pal_id * (1.0 / 256.0) + (0.5 / 256.0));
    color = vec4(pixel_color.xyz, 1.0);
}
