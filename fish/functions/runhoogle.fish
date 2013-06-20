function runhoogle
    cd ~/.hoogle
    screen -dm hoogle server -p 4000
    cd -
end
