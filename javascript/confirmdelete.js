$(document).ready(function()
{
    $('.confirm-delete').click(function()
    {
        return confirm('Are you sure you want to delete "' + $(this).attr('data-title') + '" from the Link Manager?');
    });
});