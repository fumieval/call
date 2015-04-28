#version 150
uniform mat4 projection;
uniform mat4 model;
uniform int level;
uniform float fogDensity;

in vec3 in_Position;
in vec2 in_UV;
in vec3 in_Normal;

out vec2 texUV;
out vec3 normal;
// out vec3 lightDir;
// out float fogginess;

void main(void) {
  gl_Position = projection * model * vec4(in_Position, 1.0);
  // float dist = length(model * vec4(in_Position, 1.0));
  // fogginess = clamp(1.0 / exp(dist * fogDensity), 0.0, 1.0);
  texUV = in_UV;
  normal = in_Normal;
  // lightDir = vec3(0.0, 0.0, 0.0);
}
