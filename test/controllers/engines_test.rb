require 'test_helper'

class EnginesTest < ActionDispatch::IntegrationTest
  test 'pathpuz engine' do
    get '/pathpuz'
    assert_response :success
  end
end
