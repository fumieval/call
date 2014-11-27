#version 330
#extension GL_OES_standard_derivatives : enable
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

float edgeFactor(){
    vec3 d = fwidth(vBC);
    vec3 a3 = smoothstep(vec3(0.0), d*1.5, vBC);
    return min(min(a3.x, a3.y), a3.z);
}

void main(void){
  vec3 n = mix(normal, texture(normalMap, UV).rgb * 2 - 1, normalMix);

  vec3 d = mix(vec4(1.0), texture(tex, UV).rgba, textureMix) * diffuse.rgb;
  vec3 s = vec3(0.0);

  fragColor = vec4(ambient + d + specular * s, diffuse.a * (1.0-edgeFactor()));
  
  fragColor += texture(env, envUV).rgba * envAdd;
  fragColor *= mix(vec4(1.0), texture(env, envUV).rgba, envMul);

}