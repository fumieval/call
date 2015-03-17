#version 150
out vec4 fragColor;

// Texture
in vec2 texUV;

in vec3 normal;
in vec3 lightDir;

uniform sampler2D tex;
uniform sampler2D normalMap;

uniform vec4 diffuse;
uniform vec3 specular;
uniform float normalMix;
uniform float textureMix;

void main(void){
  vec3 n = mix(normal, texture(normalMap, texUV).rgb * 2 - 1, normalMix);

  vec4 d = mix(vec4(1.0), texture(tex, texUV).rgba, textureMix) * diffuse;

  vec3 view = vec3(0,0,1);
  float brightness = 1.0;

  float shine = dot(reflect(lightDir, n), view);

  fragColor = vec4(d.rgb * brightness + specular * shine, d.a);

}
