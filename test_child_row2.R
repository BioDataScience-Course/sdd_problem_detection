library(DT)
datatable(
  cbind(' ' = '&oplus;', mtcars), escape = -2,
  options = list(
    columnDefs = list(
      list(visible = FALSE, targets = c(0, 2, 3)),
      list(orderable = FALSE, className = 'details-control', targets = 1)
    )
  ),
  callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> Toto: ' +
            d[0] + ', Jack: ' + d[2] + ', 177: ' + d[3] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"
  ))
