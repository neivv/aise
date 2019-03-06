#version 130

out vec4 color;

in vec4 frag_color;
in vec2 frag_position;

uniform sampler2D mask;

// (-1 .. 1) -> (0 .. 1)
mat3 device_coord_to_tex_coord = mat3(
    0.5, 0.0, 0.0,
    0.0, -0.5, 0.0,
    0.5, 0.5, 1.0
);

void main() {
    float alpha = texture(mask, (device_coord_to_tex_coord * vec3(frag_position.xy, 1.0)).xy).x;
    color = frag_color * vec4(alpha, alpha, alpha, alpha);
}
