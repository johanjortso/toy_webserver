function visitorCounter() {
  const xhttp = new XMLHttpRequest();
  xhttp.onload = function() {
    document.getElementById("visitorCounter").innerHTML = this.responseText;
  }
  xhttp.open("GET", "/visitors");
  xhttp.send();
}
visitorCounter();
setInterval(visitorCounter, 10000);