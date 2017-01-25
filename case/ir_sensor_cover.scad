module sensor_filter_bracket(module_height = 5,
                             total_height = 8,
                             outer_radius = 5,
                             inner_radius = 2,
                             filter_height = 0.1,
                             filter_width = 5,
                             viewport_width = 3.5) {

    difference() {
        // This is the main cylinder
        //cylinder(h=total_height, r=outer_radius, center=false, $fn=100);
        translate([-outer_radius, -outer_radius, 0]) {
            cube([outer_radius*2, outer_radius*2, total_height]);
        }
        
        // Remove a cutout for the sensor module
        translate([0, 0, -0.01]) {
            cylinder(h=module_height + 0.01, r=inner_radius, center=false, $fn=100);
        };
        
        translate([0, 0, module_height-0.01]) {
            cylinder(h=total_height - module_height + 0.02, r=viewport_width, center=false, $fn=100);
        }
        
        // Remove a cut out for the filter
        //translate([-filter_width/2, (-viewport_width-0.1), module_height+0.1]) {
        //    cube([filter_width, outer_radius*2, filter_height]);
        //};
        
        // Remove a wider cutout from the top for imaging
        //translate([0, 0, module_height - 0.01]) {
        //    cylinder(h=(total_height - module_height + 0.02), r=viewport_width, center=false, $fn=100);
            //cylinder(h=(total_height - module_height - 0.48), r1=(2), r2=(outer_radius - 1), center=false, $fn=100);
        //};
        
        translate([-(filter_width+0.5)/2, -(filter_width+0.5)/2, module_height + 0.5]) {
            cube([filter_width+0.5, filter_width+0.5, total_height-module_height+0.05]);
        }
        
        // Remove a wider cutout from the side for filter insertion and removal
        //translate([-outer_radius, (inner_radius+0.25), (module_height-1)]) {
        //    cube([outer_radius*2, outer_radius, (total_height - module_height + 1.1)]);
        //}
    }   
}

rotate([90, 0, 0]) {

sensor_filter_bracket(module_height = 6.1,
                                            total_height = 8,
                                            outer_radius = 5,
                                            inner_radius = 8.65/2,
                                            filter_height = 0.65,
                                            filter_width = 5,
                                            viewport_width = 2.25);
}