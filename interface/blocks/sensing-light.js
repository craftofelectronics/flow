var blocks = {
	
	// Set a unique name for the language
	languageName: "Fading Set",

	// inputEx fields for pipes properties
	propertiesFields: [
		// default fields (the "name" field is required by the WiringEditor):
		{"type": "string", inputParams: {"name": "name", label: "Username", typeInvite: "Enter your username." } },
		{"type": "string", inputParams: {"name": "project", label: "Project Name", typeInvite: "Name of this project." } },
		{"type": "text", inputParams: {"name": "description", label: "Description", cols: 30} },
		
		// Additional fields
		//{"type": "select", inputParams: {"name": "category", label: "Category", selectValues: ["CRAFToE", "Robotics", "Other"]} }
	],
	
	// List of node types definition
	modules: [
  {
    "name": "Read Sensor",
    "container": {
      "xtype": "WireIt.FormContainer",
      "title": "readsensor",    
      "icon": "images/tvm.png",
      "collapsible": true,
      "fields": [ 
        {"type": "select", "inputParams": {"label": "Pin", "name": "0int", "selectValues": ["A0", "A1", "A2", "A3", "A4", "A5"] } },  
      ],
      "terminals": [
      {"name": "1out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
        ],
      "legend": "Reads a sensor attached to the named pin and outputs a reading."
	   	}
	   },
	   
	   {
	     "name": "On Above Threshold",
	     "container": {
	       "xtype": "WireIt.FormContainer",
	       "title": "onabovethreshold",    
	       "icon": "images/tvm.png",
	       "collapsible": true,
	       "fields": [ 
           {"inputParams": {"label": "Threshold", "name": "0int", "value":"50", "required": true } }, 

	       ],
	       "terminals": [
           {"name": "1in", "direction": [0,-1], "offsetPosition": {"left": 100, "top": -15 }},
		   
	       {"name": "2out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
	         ],
	       "legend": "Turns on when the value read is above the threshold given."
	 	   	}
	 	   },
		   {
		     "name": "Inverter",
		     "container": {
		       "xtype": "WireIt.FormContainer",
		       "title": "offbelowthreshold",    
		       "icon": "images/tvm.png",
		       "collapsible": true,
		       "fields": [
		       ],
		       "terminals": [
	           {"name": "0in", "direction": [0,-1], "offsetPosition": {"left": 100, "top": -15 }},
			   
		       {"name": "1out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
		         ],
		       "legend": "Turns an 'on' value into an 'off' value, and visa versa."
		 	   	}
		 	   },
     {
       "name": "Print Value",
       "container": {
         "xtype": "WireIt.FormContainer",
         "title": "print_value",    
         "icon": "images/tvm.png",
         "collapsible": true,
         "fields": [ 
         ],
         "terminals": [
           {"name": "0in", "direction": [0,-1], "offsetPosition": {"left": 100, "top": -15 }},
           ],
         "legend": "Print the value received to your computer."
   	   	}
   	   },
     {
       "name": "Print And Pass Through",
       "container": {
         "xtype": "WireIt.FormContainer",
         "title": "printandpassthrough",    
         "icon": "images/tvm.png",
         "collapsible": true,
         "fields": [ 
         ],
         "terminals": [
           {"name": "0in", "direction": [0,-1], "offsetPosition": {"left": 100, "top": -15 }},
           {"name": "1out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
           ],
         "legend": "Print the value received to your computer and then pass it on."
   	   	}
   	   },
       {
         "name": "Fade",
         "container": {
           "xtype": "WireIt.FormContainer",
           "title": "fade",    
           "icon": "images/tvm.png",
           "collapsible": true,
           "fields": [ 
             {"type": "select", "inputParams": {"label": "Pin", "name": "0int", "selectValues": ["3", "5", "6", "9", "10", "11"] } },
           ],
           "terminals": [
             {"name": "1in", "direction": [0,-1], "offsetPosition": {"left": 100, "top": -15 }},
             ],
           "legend": "Fade the pin based on the input value."
     	   	}
     	   },
  ]
};