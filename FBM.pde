/**
 * 
 * PixelFlow | Copyright (C) 2017 Thomas Diewald - www.thomasdiewald.com
 * 
 * https://github.com/diwi/PixelFlow.git
 * 
 * A Processing/Java library for high performance GPU-Computing.
 * MIT License: https://opensource.org/licenses/MIT
 * 
 */

// MICROPHONE
import ddf.minim.*;

import com.thomasdiewald.pixelflow.java.DwPixelFlow;
import com.thomasdiewald.pixelflow.java.imageprocessing.DwShadertoy;

DwPixelFlow context;
DwShadertoy toy;

// Import Minim and create the object
Minim minim;
AudioInput in;

// Initialize the microphone's fft's
float wav = 0.0;
float fre = 0.0;

// Initialize the mouse
float rotX = 0.0;
float rotY = 0.0;
float rate = 0.1;

// Initialize the integer that changes colour
int col = 0;

public void settings() {
  size(800, 450, P2D);
  smooth(0);
}

public void setup() {
  
  // Initialize minim
  minim = new Minim(this);
  
  // use the getLineIn method of the Minim object to get an AudioInput
  in = minim.getLineIn();
  
  surface.setResizable(true);
  
  context = new DwPixelFlow(this);
  context.print();
  context.printGL();
  
  toy  = new DwShadertoy(context, "BufA.frag");
  
  frameRate(60);
}


public void draw() {

  // Traverse the mic data to get the fft's
  for(int i = 0; i < in.bufferSize() - 1; i++)
  {
  
    fre = ( in.left.get(i) ) * 0.5;
    wav = ( in.right.get(i) ) * 500.0;
  
  }
  
  toy.set_iMouse(rotX, rotY, float( col ), 0.0);
  toy.apply(width, height);
  toy.set_iSampleRate(fre);
  toy.apply(this.g);

  String txt_fps = String.format(getClass().getSimpleName()+ "   [size %d/%d]   [frame %d]   [fps %6.2f]", width, height, frameCount, frameRate);
  surface.setTitle(txt_fps);
  
}

void mouseDragged() {
  
  rotY += (pmouseY-mouseY) * rate;
  rotX += (mouseX-pmouseX) * rate;

}

void keyPressed() 
{

  if ( col == 0 )
  {

    if ( key == 'c' || key == 'C' )
    {

      col = 1;
      println( "Colour = " + col );
      
    }
    
  } 
  
  else if ( col == 1 )
  {

    if ( key == 'c' || key == 'C' )
    {

      col = 0;
      println( "Colour = " + col );
    }
    
  }
  
}
