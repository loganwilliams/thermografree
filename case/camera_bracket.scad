include <params.scad>

camera_offset_x = 5;
camera_offset_y = 5;

difference() {
        roundedRect([35, 30, height], 5);
    
    translate([camera_offset_x+12, camera_offset_y+13, -0.1]) {
        cylinder(h=height + 0.2, r=2.9, $fn=20);
    }
}


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