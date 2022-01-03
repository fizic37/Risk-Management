function portofoliu_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "sinteza_portofoliu").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "data_raport_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  }); 
  
}



