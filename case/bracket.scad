include <params.scad>

bracket_width = 20;
bracket_length = 40;

// 8.5 mm above the base
standover_height = 10.5;

cutout_length = 20;

wall_thickness = 2;

bracket_height = standover_height + wall_thickness;

difference() {
    cube([bracket_length, bracket_height, bracket_width]);
    
    translate([(bracket_length-cutout_length)/2, -0.1, -0.1]) {
        cube([cutout_length, standover_height+0.1, bracket_width+0.2]);
    }
    
    translate([-0.1, wall_thickness + camera_standoff_height, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness, bracket_height, bracket_width+0.2]);
    }
    
    translate([-0.1, -0.1, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness, camera_standoff_height, bracket_width+0.2]);
    }
    
    translate([(bracket_length - cutout_length)/2 + cutout_length + wall_thickness, wall_thickness + camera_standoff_height, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness + 0.1, bracket_height, bracket_width+0.2]);
    }
    
    translate([(bracket_length - cutout_length)/2 + cutout_length + wall_thickness, -0.1, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness + 0.1, camera_standoff_height, bracket_width+0.2]);
    }
}