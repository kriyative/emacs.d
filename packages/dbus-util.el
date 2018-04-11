(require 'dbus)

(dbus-introspect-xml
 :session
 "com.canonical.indicator.messages"
 "/com/canonical/indicator/messages/application")

(dbus-introspect-get-method-names
 :session
 "com.canonical.indicator.messages"
 "/com/canonical/indicator/messages/"
 "com.canonical.indicator.messages.service")

;; ("RegisterApplication" "UnregisterApplication" "ApplicationStoppedRunning" "SetStatus")

(dbus-introspect-get-method
 :session
 "com.canonical.indicator.messages"
 "/com/canonical/indicator/messages/service"
 "com.canonical.indicator.messages.service"
 "RegisterApplication")

(method ((name . "RegisterApplication")) "
      " (arg ((type . "s") (name . "desktop_id") (direction . "in"))) "
      " (arg ((type . "o") (name . "menu_path") (direction . "in"))) "
    ")
(method ((name . "SetStatus")) "
      " (arg ((type . "s") (name . "desktop_id") (direction . "in"))) "
      " (arg ((type . "s") (name . "status") (direction . "in"))) "
    ")

(dbus-call-method :session
                  "com.canonical.indicator.messages"
                  "/com/canonical/indicator/messages/thunderbird_desktop"
                  "com.canonical.indicator.messages.application"
                  "SourceAdd
)

(dbus-call-method :session
                  "com.canonical.indicator.messages"
                  "/com/canonical/indicator/messages/service"
                  "com.canonical.indicator.messages.service"
                  "ClearAttention")

com.canonical.indicator.messages.service.ClearAttention
