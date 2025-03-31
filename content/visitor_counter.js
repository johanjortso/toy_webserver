function visitorCounter() {
  const xhttp = new XMLHttpRequest();
  xhttp.onload = function() {
    document.getElementById("visitor_counter").innerHTML = this.responseText;
  }
  xhttp.open("GET", "/visitors.txt");
  //xhttp.open("GET", "https://www.w3schools.com/js/ajax_info.txt");
  xhttp.send();
}