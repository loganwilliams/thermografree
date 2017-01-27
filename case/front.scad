include <params.scad>

difference() {

union() {

difference() {
    roundedPoly([total_width, total_height, depth-10], 4);

    translate([wall_thickness, wall_thickness, height]) {
        roundedPoly([total_width - 2 * wall_thickness, total_height - 2 * wall_thickness, depth*2], 4);
    }
    
    translate([right_margin+2, bottom_margin+3, -0.1]) {
        cube([screen_width-4, screen_height-4, height+0.2]);
    }
    
    translate([right_margin, bottom_margin, 1.0]) {
        cube([screen_width, screen_height, height+0.2]);
    }
    
    translate([right_margin+screen_width-58-ribbon_cable_width, bottom_margin-5, 1.0]) {
        cube([ribbon_cable_width, 5.1, height+0.2]);
    }
    
      
}



// shell

difference() {

    translate([0, 0, 5]) {
            roundedRect([total_width, total_height, depth-5], 4);
        }
        
    translate([wall_thickness, wall_thickness, 4.9]) {
        roundedRect([left_margin+screen_width+right_margin-2*wall_thickness, top_margin+screen_height+bottom_margin-2*wall_thickness, depth + .2], 5);
    }
    
      
    
}

translate([1+standoff_od, 1+standoff_od, height]) {
    standoff(standoff_od, depth-height, 1.5, depth=5);
}

translate([total_width-1-standoff_od, 1+standoff_od, height]) {
    standoff(standoff_od, depth-height, 1.5, depth=5);
}

translate([1+standoff_od, total_height-1-standoff_od, height]) {
    standoff(standoff_od, depth-height, 1.5, depth=5);
}

translate([total_width-1-standoff_od, total_height-1-standoff_od, height]) {
    standoff(standoff_od, depth-height, 1.5, depth=5);
}

translate([total_width/2, total_height-1-standoff_od, height]) {
    standoff(standoff_od, depth-height, 1.5, depth=5);
}

}

translate([20, total_height-wall_thickness-0.1, 30]) {
    rotate([-90, 0, 0]) {
        cylinder(r=7.25/2, h=10, $fn=20);
    }
}

}

