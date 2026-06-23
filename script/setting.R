my_theme <- bs_theme(
  version = 5,
  base_font = font_google("Poppins"),
  heading_font = font_google("Montserrat")
) |>
  bs_add_rules("
/* Navbar brand text */
.navbar .navbar-brand {
  color: white !important;
  height: 60px;
  width: 200px;
  display: flex;
  align-items: center;
  justify-content: center;
}

/* Page background */
body { background-color: #9e9e9e; }

/* Navbar background and shadow */
.navbar.navbar-default {
  min-height: 60px;
  background-color: #3182bd !important;
  box-shadow: 0 4px 6px rgba(0,0,0,0.3);
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 10px;
  padding: 10px 0;
}

/* Sidebar block */
.sidebar-block {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 12px;
}

/* Brand image */
.brand-logo { height: 60px !important; width: auto !important; }
.navbar-brand img { max-height: none !important; }

/* Nav links (navbar) */
.navbar .nav-link {
  color: white !important;
  height: 40px;
  width: 200px;
  background-color: #0d2b56;
  box-shadow: 0 4px 6px rgba(0,0,0,0.3);
  border-radius: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 10px;
  margin: 5px;
}
.navbar .nav-link:hover { color: red !important; }
.navbar .nav-link.active {
  color: green !important;
  font-weight: bold;
  background-color: white !important;
}

/* Sidebar (bslib::page_sidebar) */
.sidebar {
  background-color: black !important;
  box-shadow: 0 4px 6px rgba(0,0,0,0.3);
}
.sidebar h1,
.sidebar h2,
.sidebar p,
.sidebar label,
.sidebar .nav-link { color: white !important; }

/* Tabs */
.nav-tabs .nav-link {
  color: white !important;
  font-weight: bold;
  width: 250px;
  background-color: #3182bd;
  box-shadow: 0 2px 4px rgba(0,0,0,0.2);
  border-radius: 6px;
  transition: box-shadow 0.3s ease;
}

.nav-tabs .nav-link.active {
  box-shadow: 0 4px 8px rgba(0,0,0,0.3);
  background-color: white;
  color: green !important;
  font-weight: bold;
}


.irs-min, .irs-max {
    position: absolute;
    display: block;
    cursor: default;
    color: white !important;
}

  .irs-single {
    color: white !important;   /* green */
    font-weight: bold;
  }

  .irs-grid-text {
    color: white !important;   /* blue */
    font-weight: bold;
  }

  /* Tick marks (the little vertical lines) */
  .irs-grid-pol {
    background: white !important;
  }




   
    
.dt-buttons .btn {
  background-color: #3182bd;
  color: white;
  border: none !important;
  box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.3);
}

/* Hover */
.dt-buttons .btn:hover {
  background-color: #35c6e3;
  color: black;
}

/* Active (clicked) */
.dt-buttons .btn:active,
.dt-buttons .btn.active,
.dt-buttons .btn:focus {
  background-color: white !important;
  color: green !important;
  box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.25);
  outline: none;
}

/* Remove Bootstrap blue focus ring 
.dt-buttons .btn:focus-visible {
  box-shadow: none;
}*/

table.dataTable thead th {
  background-color: #3182bd;
  color: black;
}



.col-lg-1, .col-lg-10, .col-lg-11, .col-lg-12, .col-lg-2, .col-lg-3, .col-lg-4, .col-lg-5, .col-lg-6, .col-lg-7, .col-lg-8, .col-lg-9, .col-md-1, .col-md-10, .col-md-11, .col-md-12, .col-md-2, .col-md-3, .col-md-4, .col-md-5, .col-md-6, .col-md-7, .col-md-8, .col-md-9, .col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-xs-1, .col-xs-10, .col-xs-11, .col-xs-12, .col-xs-2, .col-xs-3, .col-xs-4, .col-xs-5, .col-xs-6, .col-xs-7, .col-xs-8, .col-xs-9 {
    position: relative;
    min-height: 1px;
    padding-right: 5px;
    padding-left: 5px;
    margin:0;
}

.graphclass{
  font-family: Arial;
  /*font-weight: normal;*/
  /*border: 1px solid #2171B5;*/
  border-radius:  4px;
  /*background-color: #89CFF0; #FFFFFF;*/
  width: 100%;
  height: 100%;
  padding: 1%;
  margin:1%;
  align-items: stretch;
align-content: space-between;
   
}


.mapclass{
  font-family: Arial;
  font-weight: normal;
  border: 1px solid #2171B5;
  border-radius:  10px;
  /*background-color: #FFFFFF*/;
  width: 100%;
  height: 50%;
  padding: 1%;
  margin-right:0%;
  margin-left:1%;
   margin-bottom: 5%;
}



")