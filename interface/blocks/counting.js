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
    "name": "Count Up",
    "container": {
      "xtype": "WireIt.FormContainer",
      "title": "count",
      "icon": "images/tvm.png",
      "collapsible": true,
      "fields": [ 
        {"inputParams": {"label": "Start", "name": "0int", "value":"0", "required": true }},
        {"inputParams": {"label": "End", "name": "1int", "value":"100", "required": true }},
        {"inputParams": {"label": "Milliseconds", "name": "2int", "value":"500", "required": true }}
      ],
      "terminals": [
        {"name": "4out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
        ],
      "legend": "Count up from the start to the end, delaying for some number of millis after each count."
	   	}
	   },	

  {
    "name": "Send Values",
    "container": {
      "xtype": "WireIt.FormContainer",
      "title": "send_value",
      "icon": "images/tvm.png",
      "collapsible": true,
      "fields": [ 
        {"inputParams": {"label": "Desired Value", "name": "0int", "value":"0", "required": true }}
      ],
      "terminals": [
        {"name": "1out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
        ],
      "legend": "Send a value between 0 and 100."
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
 
  ]
};