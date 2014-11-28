#version 330
out vec4 fragColor;

// Texture
in vec2 UV;
in vec2 envUV;

varying vec3 normal;
varying vec3 lightDir;
varying vec3 vBC;

uniform vec4 diffuse;
uniform vec3 specular = vec3(0.0);
uniform vec3 ambient = vec3(0.0);
uniform float shininess;

uniform sampler2D tex;
uniform sampler2D normalMap;
uniform sampler2D env;
uniform float normalMix;
uniform float textureMix;
uniform float envAdd;
uniform float envMul;

void main(void){
  // vec3 n = mix(normal, texture(normalMap, UV).rgb * 2 - 1, normalMix);

  vec4 d = mix(vec4(1.0), texture(tex, UV).rgba, textureMix) * diffuse;
  vec3 s = vec3(0.0);

  fragColor = vec4(ambient + d.rgb + specular * s, d.a);
  
  fragColor += texture(env, envUV).rgba * envAdd;
  fragColor *= mix(vec4(1.0), texture(env, envUV).rgba, envMul);

}