include <params.scad>

camera_standoff_height = 4.7-2;

bracket_width = 10;
bracket_length = 32;

// 8.5 mm above the base
standover_height = 8.75;

cutout_length = 10.5;

wall_thickness = 2;
top_wall_thickness=1;
peg_distance = 15.25;

bracket_height = standover_height + top_wall_thickness;

difference() {
    cube([bracket_length, bracket_height, bracket_width]);
    
    translate([(bracket_length-cutout_length)/2, -0.1, -0.1]) {
        cube([cutout_length, standover_height+0.1, bracket_width+0.2]);
    }
    
    translate([-0.1, wall_thickness + camera_standoff_height, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness + 0.1, bracket_height, bracket_width+0.2]);
    }
    
    translate([-0.1, -0.1, -0.1]) {
        cube([(bracket_length - peg_distance)/2 - wall_thickness + 0.1, camera_standoff_height + 0.1, bracket_width+0.2]);
    }
        
    translate([(bracket_length - cutout_length)/2 + cutout_length + wall_thickness, wall_thickness + camera_standoff_height, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness + 0.1, bracket_height, bracket_width+0.2]);
    }
    
    translate([(bracket_length - peg_distance)/2 + peg_distance + wall_thickness, -0.1, -0.1]) {
        cube([(bracket_length - cutout_length)/2 - wall_thickness, camera_standoff_height+0.1, bracket_width+0.2]);
    }
   
    translate([(bracket_length - peg_distance)/2, -0.1, -0.1]) {
        cube([peg_distance, camera_standoff_height+0.1, bracket_width+0.2]);
    }
    
    translate([3, 10, bracket_width/2]) {
        rotate([90, 0, 0]) {
            cylinder(r=2, h=10, $fn=20);
        }
    }
    
    translate([bracket_length-3, 10, bracket_width/2]) {
        rotate([90, 0, 0]) {
            cylinder(r=2, h=10, $fn=20);
        }
    }
    
    translate([bracket_length/2, 10, bracket_width/2]) {
        rotate([90, 0, 0]) {
            cylinder(r=3.5, h=10, $fn=20);
        }
    }
    
    /*translate([bracket_length/2 - cutout_length/2, -0.1, -0.1]) {
        cube([cutout_length, bracket_height+0.2, bracket_width/2+0.1-1]);
    }
    
    translate([bracket_length/2 - cutout_length/2, -0.1, bracket_width/2+1]) {
        cube([cutout_length, bracket_height+0.2, bracket_width/2+0.1-1]);
    }*/
}

    