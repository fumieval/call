#version 330
out vec4 fragColor;

// Texture
in vec2 UV;
in vec2 envUV;

in vec3 normal;
in vec3 lightDir;

uniform vec4 diffuse;
uniform vec3 specular = vec3(0.0);
uniform vec3 ambient = vec3(0.0);
uniform float shininess;
uniform float normalMix;
uniform float textureMix;
uniform float envAdd;
uniform float envMul;

uniform sampler2D env;
uniform sampler2D tex;

void main(void){
  // vec3 n = mix(normal, texture(normalMap, UV).rgb * 2 - 1, normalMix);

  vec4 d = mix(vec4(1.0), texture(tex, UV).rgba, textureMix) * diffuse;

  fragColor = vec4(ambient + d.rgb, d.a);
  
  fragColor += texture(env, envUV).rgba * envAdd;
  fragColor *= mix(vec4(1.0), texture(env, envUV).rgba, envMul);

}