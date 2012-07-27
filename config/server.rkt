#hash((module . "flow.module")
      (server-port . 8000)
      (base-url . "http://localhost:8000")
      (remote-url . "https://raw.github.com/craftofelectronics/flow/master")
      (versions . #hash(("server.rkt"          . 100)
                        ("arduino.rkt"         . 100)
                        ("arduinouno.rkt"      . 100)
                        ("flow.module"         . 101)
                        ;; The JS modules need to be a directory, not a file...
                        ("ifttt.js"            . 100)
                   ))
      (interfaces . #hash(("_path"             . "interface/blocks")
                          ("Flow"              . #hash(("file"        . "ifttt.js")
                                                       ("version"     . 100)))
                          ))
                          
      (update-paths . #hash(("server.rkt"      . "config")
                            ("arduino.rkt"     . "config")
                            ("arduinouno.rkt"  . "config")
                            ("flow.module"     . "occam/flow")
                            ("ifttt.js"        . "interface")
                            ))
      (update-urls . #hash(("occam/flow" .
                            "https://raw.github.com/craftofelectronics/flow/master/occam/flow/")
                           ("interface" .
                            "https://raw.github.com/craftofelectronics/flow/master/interface/")
                           ("config" .
                            "https://raw.github.com/craftofelectronics/flow/master/config/")))
      
      (max-windows-com-port . 15)
      )
        
