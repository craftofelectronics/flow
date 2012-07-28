var blocks = {
	
	// Set a unique name for the language
	languageName: "Simple Set",

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
      "title": "read_sensor",
      "icon": "images/tvm.png",
      "collapsible": true,
      "fields": [ 
        {"type": "select", "inputParams": {"label": "Pin", "name": "0int", "selectValues": ["A0", "A1", "A2", "A3", "A4", "A5"] } },
      ],
      "terminals": [
        {"name": "1out", "direction": [0,1], "offsetPosition": {"left": 100, "bottom": -15}}
        ],
      "legend": "Read my sensor and output its value."
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