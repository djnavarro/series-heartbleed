#include <Rcpp.h>
using namespace Rcpp;


namespace trmt {

  // wrap position to grid
  int wrap(int pos, int sz) {
    if(pos < 0) pos = pos + sz;
    if(pos >= sz) pos = pos - sz;
    return pos;
  }

  // move the turmite in the direction it is facing
  IntegerVector step(IntegerVector position, int facing, int pixels) {
    
    // move
    if(facing == 0) position[1]++;
    if(facing == 1) position[0]++;
    if(facing == 2) position[1]--;
    if(facing == 3) position[0]--;
    
    position[0] = trmt::wrap(position[0], pixels); 
    position[1] = trmt::wrap(position[1], pixels); 
    
    return position;
  }
  
  
  // turmite function
  IntegerMatrix run_turmite(int pixels, int iter, int step_size) {
    
    IntegerVector pos = {pixels/2, pixels/2};
    int old_state = 0;
    int new_state = 0;
    int facing = 0;
    int color;

    IntegerMatrix grid(pixels, pixels); // initially zero
    IntegerMatrix cols(pixels, pixels); // initially zero
    
    for(int t = 0; t < iter; t++) {
      
      color = cols(pos[0], pos[1]);
      
      if(color == 0 & old_state == 0) {
        facing = (facing + 1) % 4;             // turn clockwise
        for(int s = 0; s < step_size; s++) {
          grid(pos[0], pos[1]) = grid(pos[0], pos[1]) + 1; 
          cols(pos[0], pos[1]) = 1; 
          pos = trmt::step(pos, facing, pixels); // move forward
        }
        new_state = 0;                         // stay in state 0
      }
      
      if(color == 0 & old_state == 1) {
        for(int s = 0; s < step_size; s++) {
          grid(pos[0], pos[1]) = grid(pos[0], pos[1]) + 1; 
          cols(pos[0], pos[1]) = 0; 
          pos = trmt::step(pos, facing, pixels); // move forward
        }
        new_state = 0;                         // swap to state 0
      }
      
      if(color > 0 & old_state == 0) {
        facing = (facing + 1) % 4;             // turn clockwise
        for(int s = 0; s < step_size; s++) {
          grid(pos[0], pos[1]) = grid(pos[0], pos[1]) + 1; 
          cols(pos[0], pos[1]) = 1; 
          pos = trmt::step(pos, facing, pixels); // move forward
        }
        new_state = 1;                         // swap to state 1
      }
      
      if(color > 0 & old_state == 1) {
        for(int s = 0; s < step_size; s++) {
          grid(pos[0], pos[1]) = grid(pos[0], pos[1]) + 1; 
          cols(pos[0], pos[1]) = 0; 
          pos = trmt::step(pos, facing, pixels); // move forward
        }
        new_state = 1;                         // stay in state 1
      }
      
      old_state = new_state;
    }
    
    return grid;
  }

}


// turmite function to be called from R
// [[Rcpp::export]]
IntegerMatrix turmite(int pixels, int iter, int step_size) {
  IntegerMatrix grid = trmt::run_turmite(pixels, iter, step_size);
  return grid;
}


