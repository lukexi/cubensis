#version 330 core

uniform vec4 uColor;

in vec3 vNormal;
in vec3 vPosition;

out vec4 color;

const   vec3 lightColor = vec3(1);
const   vec3 lightPosition = vec3(0,0,1);
const   float ambient = 0.2;

void main(void) {

  vec3 normal = normalize(vNormal);

  vec3 surfaceToLight = normalize(lightPosition - vPosition);

  float diffuseCoefficient = max(ambient, dot(normal, surfaceToLight));
  vec3 diffuseLit = diffuseCoefficient * uColor.rgb * lightColor;

  color = vec4(diffuseLit, uColor.a);
}
