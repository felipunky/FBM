#version 150

#define SAMPLER0 sampler2D // sampler2D, sampler3D, samplerCube
#define SAMPLER1 sampler2D // sampler2D, sampler3D, samplerCube
#define SAMPLER2 sampler2D // sampler2D, sampler3D, samplerCube
#define SAMPLER3 sampler2D // sampler2D, sampler3D, samplerCube

uniform SAMPLER0 iChannel0; // image/buffer/sound    Sampler for input textures 0
uniform SAMPLER1 iChannel1; // image/buffer/sound    Sampler for input textures 1
uniform SAMPLER2 iChannel2; // image/buffer/sound    Sampler for input textures 2
uniform SAMPLER3 iChannel3; // image/buffer/sound    Sampler for input textures 3

uniform vec3  iResolution;           // image/buffer          The viewport resolution (z is pixel aspect ratio, usually 1.0)
uniform float iTime;                 // image/sound/buffer    Current time in seconds
uniform float iTimeDelta;            // image/buffer          Time it takes to render a frame, in seconds
uniform int   iFrame;                // image/buffer          Current frame
uniform float iFrameRate;            // image/buffer          Number of frames rendered per second
uniform vec4  iMouse;                // image/buffer          xy = current pixel coords (if LMB is down). zw = click pixel
uniform vec4  iDate;                 // image/buffer/sound    Year, month, day, time in seconds in .xyzw
uniform float iSampleRate;           // image/buffer/sound    The sound sample rate (typically 44100)
uniform float iChannelTime[4];       // image/buffer          Time for channel (if video or sound), in seconds
uniform vec3  iChannelResolution[4]; // image/buffer/sound    Input texture resolution for each channel

// EPS defines the epsilon that we use as a minimum for going out of our trace
// function ray or for defining our shading's function sha 3D treshold
#define EPS       0.001

// The STEPS integer stores the number of rays that we shoot at our scene,
// more means a better resolution(specially at the edges) but it also
// messes up our frame rate as it means many more calculations
#define STEPS        64

// The FAR float macro defines where should we stop tracing according to the
// distance from our camera to the 3D scene
#define FAR        70.0

// Simple trigonometric trick to get pi
#define PI acos( -1.0 )

// Double the pi's value to get a full rotation in our camera matrix
#define TPI     PI * 2.

// The COLOUR macro defines how should we colour our scene, if it is defined
// the function will return a different value than if its not. Simply comment
// or uncomment the next line to see what I mean
#define COLOUR

// Samples our microphone's or music's frequency, it is stored in a texture
// as all of the inputs here in shadertoy so we must call the texture()
// function with input 1 our Channel, input 2 we define the frequency that we
// want to sample in two dimensions after we ask only for the x part of the
// texture as it is a texture it contains 3 values and we only want one float
// after we multiply by 0.1 to obtain a less strong value for our purposes
#define WAV texture( iChannel0, vec2( 0.0, 0.25 ) ).x * 0.1

// Creates a random number from 0 to 1 ( sine's fractional part so for
// example if we input sin( 3 ) we obtain 0.29552020666, its fract is only
// 29552020666 ) it is not really a random value but more of a
// pseudo-stochastic one, as if we will get the same value for the same seed
// note that this is not entirely true as in different machines we have
// different floating point precision in the GPU so the number will differ
// from one computer to another.
float hash( vec2 a )
{
    
    return fract( sin( a.x * 3433.8 + a.y * 3843.98 ) * 45933.8 );
    
}

// Divides the 2D space in tiles than those tiles are asigned a random colour
// than we interpolate using GLSL's mix() function to interpolate to combine
// the different random values of each tile into a 2D texture. Uncomment the

float noise( vec2 uv )
{
    
    vec2 lv = fract( uv );
    lv = lv * lv * ( 3.0 - 2.0 * lv );
    vec2 id = floor( uv );
    
    float bl = hash( id );
    float br = hash( id + vec2( 1, 0 ) );
    float b = mix( bl, br, lv.x );
    
    float tl = hash( id + vec2( 0, 1 ) );
    float tr = hash( id + vec2( 1 ) );
    float t = mix( tl, tr, lv.x );
    
    float c = mix( b, t, lv.y );
    
    return c;
    
}

// Adds the noise's function input ( uv ) by different octaves than divides the
// whole function by it's negative octave to create a pattern that defines a
// Fractional Brownian Motion see
// https://en.wikipedia.org/wiki/Fractional_Brownian_motion and
// https://thebookofshaders.com/13/
// uncomment the next line to see what I mean
//#define FBM

float fbm( vec2 uv )
{
    
    float f = 0.0;
    
#ifdef FBM
    
    f = noise( uv * 4.0 );
    f += noise( uv * 8. ) * 0.5;
    f += noise( uv * 16. ) * 0.25;
    f += noise( uv * 32. ) * 0.125;
    f += noise( uv * 64. ) * 0.0625;
    //f += noise( uv * 126. ) * 0.03125;
    f /= 2.0;
    
#else
    
    f = noise( uv * 4.0 );
    f += noise( uv * 8. ) * 0.5; uv += iSampleRate * 1.1;
    f += noise( uv * 16. ) * 0.25; uv -= iSampleRate * 1.2;
    f += noise( uv * 32. ) * 0.125; uv += iSampleRate * -1.1;
    f += noise( uv * 64. ) * 0.0625; uv -= iSampleRate * 1.4;
    //f += noise( uv * 126. ) * 0.03125;
    f /= 2.0;
    
#endif
    
    return f;
    
}

// Samples the fbm to the y direction of the plane
// We add domain distortion to get the curly looking fluid
// http://www.iquilezles.org/www/articles/warp/warp.htm
float hei( vec2 uv )
{
    
    // We use the mod function to repeat the iTime as we get artifacts
    // when we have a very high float number from iTime
    return fbm( uv * 0.1 + fbm( uv * 0.1 + mod( iTime * 0.05, 100.0 ) ) );
    
}

// Samples the fbm at different inputs to get a different colour
float pat( vec2 uv, out vec3 pa )
{
    
    pa = vec3( hei( uv * 4.0 ) * 2.0, hei( uv * 2.0 ),
              hei( uv - 3.0 )
              );
    
    return 1.0;
    
}

// Defines a Signed Distance Function if its inside the surface it returs 0
// else it returns a positive number, although this is a float that we need
// to output it is important for our shading to return a 2nd value therefore
// it is a vec2, this way we can change our shading according to the index
// that we assing to the SDF
// https://en.wikipedia.org/wiki/Signed_distance_function
vec2 map(vec3 p)
{
    
    // Here we input the xz of our position, that means our y direction
    float a = hei( p.xz );
    
    // Here we create the plane function p.y + 1.0 or in this case p.y + a
    // as we want to create a heightmap from our Fractional Brownian Motion
    vec2 rip = vec2( p.y + a, 0.0 );
    
    return rip;
}

// We define the perpendiculars according to sampling the Signed Distance
// Function and doing Numerical Differentiation aka we find the derivatives
// https://en.wikipedia.org/wiki/Numerical_differentiation
vec3 nor( vec3 p )
{
    
    vec2 e = vec2( EPS, 0.0 );
    return normalize( vec3( map( p + e.xyy ).x - map( p - e.xyy ).x,
                           map( p + e.yxy ).x - map( p - e.yxy ).x,
                           map( p + e.yyx ).x - map( p - e.yyx ).x
                           )
                     );
    
}

// We trace a ray from its Ray Origin(ro) and to its Ray Direction(rd) if we
// get close enough to our Signed Distance Function we stop, this distance is
// defined by EPS aka epsilon. We also stop if the distance of the ray is more
// than the defined maximum length aka FAR
float ray( vec3 ro, vec3 rd, out float d )
{
    
    float t = 0.0;
    for( int i = 0; i < STEPS; ++i )
    {
        
        d = map( ro + rd * t ).x;
        if( d < EPS || t > FAR ) break;
        t += d;
        
    }
    
    return t;
    
}

// We trace a ray from our Ray Origin(ro) to our Ray Direction(rd) in a way
// similar to our ray function but we increment according to the intersections
// of our ray
// http://www.iquilezles.org/www/articles/rmshadows/rmshadows.htm
float softShadows( in vec3 ro, in vec3 rd )
{
    
    float res = 1.0;
    for( float t = 0.1; t < 8.0; ++t )
    {
        
        float h = map( ro + rd * t ).x;
        res = min( res, 4.0 * h / t );
        t += h;
        
    }
    
    return res;
    
}

// We compute the colours according to different simulated phenomena such as
// diffuse, ambient, specularity
// Variable definitions:
// col = to the output RGB channels we are calculating
// d = our Signed Distance Function
// t = our ray's distance
// p = our point in space
// n = our numerical gradient aka derivatives aka perpendicular of our surface
// a = we sample our height function in y to get a value to interpolate for
// our colour in line 255, 256
// lig = our lights position, note that we must normalize as we dont want a
// direction but only a point in space
// blig = another light that we define by negating our original light
// pa = the initialization of our 3D vector that we input in our pat for
// pattern function to get different colours for our different RGB channels
// co = this is a way to get a different output from a function than the
// return way, see line 103 to see how to define it in the inputs of our
// function
// amb = our ambient light, we use our y direction in the normals to fake a
// sun's parallel rays
// dif = we use the dot product from our normals and our light to get the
// diffuse component we must use the max function to not get a value less
// than 0 as this is incorrect
// spe = our specular component we use the same process of our diffuse
// component but instead we over load it by the clamp and power functions to
// get a much brighter result that simulates the bright reflection of a light
// into a surface
// speO = specular component two, we use here instead of the dot product from
// the light and the normals, the back light and the normals
// col *= sqrt( col ) a cheap gamma correction
vec3 sha( vec3 ro, vec3 rd )
{
    
    vec3 col = vec3( 0.0 );
    float d = 0.0;
    float t = ray( ro, rd, d );
    vec3 p = ro + rd * t;
    vec3 n = nor( p );
    float a = hei( p.xz );
    vec3 lig = normalize( vec3( 1.0, 0.8, 0.6 ) );
    vec3 blig = vec3( -lig.x, -lig.y, -lig.z );
    vec3 pa = vec3( 0 );
    float co = pat( p.xz, pa );
    float sha = softShadows( p, lig );
    
    float amb = 0.5 + 0.5 * n.y;
    float dif = max( 0.0, dot( n, lig ) );
    float bac = max( 0.0, 0.5 + 0.2 * dot( n, blig ) );
    float spe = pow( clamp( dot( n, lig ), 0.0, 1.0 ), 16.0 );
    float speO = pow( clamp( dot( n, blig ), 0.0, 1.0 ), 16.0 );
    
    col += amb;
    col += 1.0 * blig;
    col += vec3( 0.4 ) * dif * sha;
    col *= 0.5 + vec3( 0.0, 0.5, 0.4 );
    col += 1.0 * spe * speO;
    
    // We have different colour values if the macro COLOUR is defined we compute
    // the fbm function with slightly different inputs to get a different R,G and
    // B colour
    if( iMouse.z == 0.0 )
    {
        
        col *= pa * 2.0;
    
    }
    
    // If we have the macro COLOUR not defined we compute the colours according to
    // the height's function hei so that in the valleys we have one colour and in
    // our mountains we have another
    else if( iMouse.z == 1.0 )
    {
        
        col += vec3( 0.0, 0.1, 0.3 );
        col *= mix( vec3( 1.0, 0.0, 0.0 ), vec3( 2.0, 1.0, 0.1 ), a );
        col *= mix( vec3( 0 ), vec3( 0.7, 0.2, 0.1 ), a );
        col *= 5.0;
        
    }
    
    col *= sqrt( col );
    
    return col;
    
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Normalized pixel coordinates (from -1 to 1)
    vec2 uv = ( -iResolution.xy + 2.0 * fragCoord ) / iResolution.y;
    
    // We create a camera matrix and we create the mou variable that lets us
    // get input from our mouse
    vec2 mou = -iMouse.xy / iResolution.xy;
    vec3 ro = 3.0 * vec3( sin( mou.x * TPI ), 1.0, cos( -mou.x * TPI ) );
    vec3 ww = normalize( vec3( 0.0 ) - ro );
    vec3 uu = normalize( cross( vec3( 0.0, 1.0, 0.0 ), ww ) );
    vec3 vv = normalize( cross( ww, uu ) );
    vec3 rd = normalize( uv.x * uu + uv.y * vv + 1.5 * ww );
    
    // We intialize the values for our tracing function ray and our p for
    // position and our n for normals or gradient
    float d = 0.0;
    float t = ray( ro, rd, d );
    vec3 p = ro + rd * t;
    vec3 n = nor( p );
    vec3 col = vec3( 0 );
    
    // If the value of d for Distance Function is smaller than our minimum
    // value aka EPS or epsilon we call our shade function sha else we colour
    // the pixels to 0 or black
    
#ifdef FBM
    
    col = vec3( fbm( uv ) );
    
#else
    
    col = d < EPS ? sha( ro, rd ) : vec3( 0 );
    
#endif
    
    // Output to screen
    fragColor = vec4(col,1.0);
}
