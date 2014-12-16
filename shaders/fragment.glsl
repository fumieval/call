#version 330
out vec4 fragColor;

// Texture
in vec2 UV;
in vec2 envUV;

in vec3 normal;
in vec3 lightDir;

uniform sampler2D env;
uniform sampler2D tex;

uniform vec4 diffuse;
uniform float normalMix;
uniform float textureMix;
uniform float envAdd;
uniform float envMul;

void main(void){
  // vec3 n = mix(normal, texture(normalMap, UV).rgb * 2 - 1, normalMix);

  vec4 d = mix(vec4(1.0), texture(tex, UV).rgba, textureMix) * diffuse;

  fragColor = vec4(d.rgb, d.a);

  fragColor += texture(env, envUV).rgba * envAdd;
  fragColor *= mix(vec4(1.0), texture(env, envUV).rgba, envMul);

}
