Shiny.addCustomMessageHandler("setAppShell", function(isLoggedIn) {
  document.body.classList.toggle("app-shell", !!isLoggedIn);
});
