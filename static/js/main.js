function filter(fval)
{
    var filter = new RegExp(fval, 'i');
    $('#plist-table tbody tr').each(function() {
        $(this).show();
    });
    $('#plist-table tbody tr').each(function() {
        if ( ! filter.test($(this).html()))
        {
            $(this).hide();
        }
    });
}

function package_details(url)
{
    $.ajax({
        data: '',
        type: 'get',
        url: url,
        success: function(res)
        {
            $('#package-details').html(res);

            $('.starlist').each(function() {
                if ($(this).html() == '??')
                {
                    var stars = $('#package-details .stars').html();
                    $(this).html(stars);
                }
            });
        }
    });
}

$(document).ready(function() {

    var table = $('#plist-table').stupidtable();
    var th_to_sort = table.find("thead th").eq(1);
    th_to_sort.stupidsort('desc');

    var last_search = window.location.href.replace(/.*?#/, '');
    if (/^\/saved/.test(last_search))
    {
        package_details(last_search.replace(/^\/saved/, ''));
    }
    else
    {
        window.location.href='#/saved/package/sluglisp';
        package_details('/package/sluglisp');
    }

    $('.package-name').click(function() {
        window.location.href='#/saved' + $(this).attr('href');
        package_details($(this).attr('href'));

        $(this).parent().parent().find('.starlist').html('??');

        return false;
    });

    $('#filter').change(function() {
        filter($(this).val());
    });

    $('#go').click(function() {
        filter($('#filter').val());
    });

    $('#reset').click(function() {
        $('#filter').val('');
        filter('');
    });
});
