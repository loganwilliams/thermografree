include <params.scad>

difference() {
        roundedRect([left_margin+screen_width+right_margin, top_margin+screen_height+bottom_margin, height], 5);
    
    
    translate([1+standoff_od, 1+standoff_od, -0.1]) {
        cylinder(h=height + .2, r=2, $fn=20);
    }
    
    translate([total_width-(1+standoff_od), 1+standoff_od, -0.1]) {
        cylinder(h=height + .2, r=2, $fn=20);
    }
    
    translate([1+standoff_od, total_height-(1+standoff_od), -0.1]) {
        cylinder(h=height + .2, r=2, $fn=20);
    }
    
    translate([total_width-(1+standoff_od), total_height-(1+standoff_od), -0.1]) {
        cylinder(h=height + .2, r=2, $fn=20);
    }
    
    translate([total_width/2, (1+standoff_od), -0.1]) {
        cylinder(h=height + .2, r=2, $fn=20);
    }
    
    translate([camera_offset_x+12, camera_offset_y+13, -0.1]) {
        cylinder(h=height + 0.2, r=2.9, $fn=20);
    }
    
    translate([sensor_offset_x+10, sensor_offset_y+10, -0.1]) {
        cylinder(h=height + 0.2, r=3.5, $fn=20);
    }
}

//camera

translate([camera_offset_x, camera_offset_y, height]) {

    translate([1.3, 1.3, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }

    translate([21+1.3, 1.3, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }

    translate([1.3, 12.4+1.3, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }

    translate([21+1.3, 12.4+1.3, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }

}

//sensor

translate([sensor_offset_x, sensor_offset_y, height]) {
    difference() {
        cube([20, 20, 1.9]);
        
        translate([(20-15.2)/2, (20-15.2)/2, -1]) {
            cube([15.2, 15.2, 6.1]);
        }
        
        translate([10, 10, -2.1]) {
            cylinder(r=3.5, h=10, $fn=20);
        }
    }
    
    translate([10, -3.01, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }
    
    translate([10, 3.01+20, 0]) {
        standoff(3, camera_standoff_height, 1, depth=4.5);
    }
}

// draw ridges for fitting

translate([15, total_height - 2*wall_thickness-0.1, height]) {
    cube([total_width-30, wall_thickness, wall_thickness]);
}

translate([15, wall_thickness+0.1, height]) {
    cube([total_width/2 - 30, wall_thickness, wall_thickness]);
}

translate([15 + total_width/2, wall_thickness+0.1, height]) {
    cube([total_width/2 - 30, wall_thickness, wall_thickness]);
}

translate([wall_thickness+0.1, 15, height]) {
    cube([wall_thickness, total_height-30, wall_thickness]);
}

translate([total_width - 2*wall_thickness-0.1, 15, height]) {
    cube([wall_thickness, total_height-30, wall_thickness]);
}