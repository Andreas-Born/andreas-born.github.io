    function myFunction() {
    var x = document.getElementById("mynav");
    if (x.className === "navigation") {
        x.className += " responsive";
    } else {
        x.className = "navigation";
    }
}