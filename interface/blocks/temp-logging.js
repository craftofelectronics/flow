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
    "name": "Temp Logger",
    "container": {
      "xtype": "WireIt.FormContainer",
      "title": "templogger",    
      "icon": "images/tvm.png",
      "collapsible": true,
      "fields": [ 
          {"inputParams": {"label": "Log Period", "name": "0int", "value":"15", "required": true } }, 
          {"inputParams": {"label": "Current Hour", "name": "1int", "value":"3", "required": true } }, 
          {"inputParams": {"label": "Current Minutes", "name": "2int", "value":"14", "required": true } }, 
      ],
      "terminals": [
        ],
      "legend": "Takes a reading every <log period> minutes. Also, tell it what time it is."
	   	}
	   },
  ]
};