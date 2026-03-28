#include <algorithm>
#include <cmath>
#include <cstdint>
#include <deque>
#include <functional>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace optibook_like {

using PriceTicks = std::int64_t;

constexpr PriceTicks kTickScale = 10;  // One decimal place, e.g. 43.9 -> 439.

enum class Side { Bid, Ask };
enum class OrderType { Limit, IOC };

struct Trade {
    std::uint64_t trade_id {};
    std::string instrument;
    std::uint64_t resting_order_id {};
    std::uint64_t incoming_order_id {};
    std::string buyer;
    std::string seller;
    PriceTicks price_ticks {};
    int volume {};
};

struct Order {
    std::uint64_t order_id {};
    std::string instrument;
    std::string owner;
    Side side {Side::Bid};
    OrderType type {OrderType::Limit};
    PriceTicks price_ticks {};
    int remaining_volume {};
    std::uint64_t sequence {};
};

struct BookLevel {
    PriceTicks price_ticks {};
    int total_volume {};
};

struct BookSnapshot {
    std::vector<BookLevel> bids;
    std::vector<BookLevel> asks;
};

std::string side_to_string(Side side) {
    return side == Side::Bid ? "bid" : "ask";
}

std::string order_type_to_string(OrderType type) {
    return type == OrderType::Limit ? "limit" : "ioc";
}

Side parse_side(const std::string& raw) {
    if (raw == "bid" || raw == "buy") {
        return Side::Bid;
    }
    if (raw == "ask" || raw == "sell") {
        return Side::Ask;
    }
    throw std::runtime_error("Unknown side: " + raw);
}

OrderType parse_order_type(const std::string& raw) {
    if (raw == "limit") {
        return OrderType::Limit;
    }
    if (raw == "ioc") {
        return OrderType::IOC;
    }
    throw std::runtime_error("Unknown order type: " + raw);
}

PriceTicks to_ticks(double price) {
    return static_cast<PriceTicks>(std::llround(price * static_cast<double>(kTickScale)));
}

double from_ticks(PriceTicks ticks) {
    return static_cast<double>(ticks) / static_cast<double>(kTickScale);
}

std::string format_price(PriceTicks ticks) {
    std::ostringstream out;
    out << std::fixed << std::setprecision(1) << from_ticks(ticks);
    return out.str();
}

class OrderBook {
public:
    explicit OrderBook(std::string instrument_id)
        : instrument_id_(std::move(instrument_id)) {}

    std::vector<Trade> submit(Order order, std::uint64_t& next_trade_id) {
        if (order.remaining_volume <= 0) {
            throw std::runtime_error("Order volume must be positive");
        }

        std::vector<Trade> trades;
        match(order, next_trade_id, trades);

        if (order.remaining_volume > 0 && order.type == OrderType::Limit) {
            rest(std::move(order));
        }

        return trades;
    }

    bool cancel(std::uint64_t order_id) {
        auto it = order_index_.find(order_id);
        if (it == order_index_.end()) {
            return false;
        }

        const Handle handle = it->second;
        if (handle.side == Side::Bid) {
            auto level_it = bids_.find(handle.price_ticks);
            erase_from_level(level_it, handle.order_it, bids_);
        } else {
            auto level_it = asks_.find(handle.price_ticks);
            erase_from_level(level_it, handle.order_it, asks_);
        }

        order_index_.erase(it);
        return true;
    }

    BookSnapshot snapshot(std::size_t depth = 5) const {
        BookSnapshot snap;

        std::size_t count = 0;
        for (const auto& [price, orders] : bids_) {
            if (count++ >= depth) {
                break;
            }
            snap.bids.push_back(BookLevel{price, aggregate_volume(orders)});
        }

        count = 0;
        for (const auto& [price, orders] : asks_) {
            if (count++ >= depth) {
                break;
            }
            snap.asks.push_back(BookLevel{price, aggregate_volume(orders)});
        }

        return snap;
    }

    const std::string& instrument_id() const {
        return instrument_id_;
    }

private:
    using PriceLevelOrders = std::list<Order>;
    using BidMap = std::map<PriceTicks, PriceLevelOrders, std::greater<>>;
    using AskMap = std::map<PriceTicks, PriceLevelOrders, std::less<>>;

    struct Handle {
        Side side;
        PriceTicks price_ticks;
        PriceLevelOrders::iterator order_it;
    };

    std::string instrument_id_;
    BidMap bids_;
    AskMap asks_;
    std::unordered_map<std::uint64_t, Handle> order_index_;

    static int aggregate_volume(const PriceLevelOrders& orders) {
        int total = 0;
        for (const auto& order : orders) {
            total += order.remaining_volume;
        }
        return total;
    }

    template <typename PriceMap>
    void erase_from_level(typename PriceMap::iterator level_it,
                          typename PriceLevelOrders::iterator order_it,
                          PriceMap& side_map) {
        if (level_it == side_map.end()) {
            return;
        }

        level_it->second.erase(order_it);
        if (level_it->second.empty()) {
            side_map.erase(level_it);
        }
    }

    bool crosses(const Order& incoming, PriceTicks best_opposite_price) const {
        if (incoming.side == Side::Bid) {
            return incoming.price_ticks >= best_opposite_price;
        }
        return incoming.price_ticks <= best_opposite_price;
    }

    void match(Order& incoming, std::uint64_t& next_trade_id, std::vector<Trade>& trades) {
        if (incoming.side == Side::Bid) {
            while (incoming.remaining_volume > 0 && !asks_.empty()) {
                auto best_ask_it = asks_.begin();
                if (!crosses(incoming, best_ask_it->first)) {
                    break;
                }

                match_against_level(incoming, best_ask_it, next_trade_id, trades, asks_);
            }
        } else {
            while (incoming.remaining_volume > 0 && !bids_.empty()) {
                auto best_bid_it = bids_.begin();
                if (!crosses(incoming, best_bid_it->first)) {
                    break;
                }

                match_against_level(incoming, best_bid_it, next_trade_id, trades, bids_);
            }
        }
    }

    template <typename PriceMap>
    void match_against_level(Order& incoming,
                             typename PriceMap::iterator level_it,
                             std::uint64_t& next_trade_id,
                             std::vector<Trade>& trades,
                             PriceMap& side_map) {
        auto& level_orders = level_it->second;

        while (incoming.remaining_volume > 0 && !level_orders.empty()) {
            auto resting_it = level_orders.begin();
            Order& resting = *resting_it;

            const int matched_volume = std::min(incoming.remaining_volume, resting.remaining_volume);
            incoming.remaining_volume -= matched_volume;
            resting.remaining_volume -= matched_volume;

            Trade trade;
            trade.trade_id = next_trade_id++;
            trade.instrument = instrument_id_;
            trade.resting_order_id = resting.order_id;
            trade.incoming_order_id = incoming.order_id;
            trade.price_ticks = resting.price_ticks;
            trade.volume = matched_volume;

            if (incoming.side == Side::Bid) {
                trade.buyer = incoming.owner;
                trade.seller = resting.owner;
            } else {
                trade.buyer = resting.owner;
                trade.seller = incoming.owner;
            }

            trades.push_back(std::move(trade));

            if (resting.remaining_volume == 0) {
                order_index_.erase(resting.order_id);
                level_orders.erase(resting_it);
            }
        }

        if (level_orders.empty()) {
            side_map.erase(level_it->first);
        }
    }

    void rest(Order order) {
        if (order.side == Side::Bid) {
            auto& level = bids_[order.price_ticks];
            level.push_back(std::move(order));
            auto order_it = std::prev(level.end());
            order_index_[order_it->order_id] = Handle{Side::Bid, order_it->price_ticks, order_it};
        } else {
            auto& level = asks_[order.price_ticks];
            level.push_back(std::move(order));
            auto order_it = std::prev(level.end());
            order_index_[order_it->order_id] = Handle{Side::Ask, order_it->price_ticks, order_it};
        }
    }
};

class Exchange {
public:
    void add_instrument(const std::string& instrument_id) {
        books_.try_emplace(instrument_id, instrument_id);
    }

    std::uint64_t submit_order(const std::string& owner,
                               const std::string& instrument_id,
                               Side side,
                               double price,
                               int volume,
                               OrderType type) {
        OrderBook& book = get_book(instrument_id);

        Order order;
        order.order_id = next_order_id_++;
        order.instrument = instrument_id;
        order.owner = owner;
        order.side = side;
        order.type = type;
        order.price_ticks = to_ticks(price);
        order.remaining_volume = volume;
        order.sequence = next_sequence_++;

        const auto trades = book.submit(order, next_trade_id_);
        for (const auto& trade : trades) {
            trades_.push_back(trade);
            print_trade(trade);
        }

        return order.order_id;
    }

    bool cancel_order(const std::string& instrument_id, std::uint64_t order_id) {
        return get_book(instrument_id).cancel(order_id);
    }

    void print_book(const std::string& instrument_id, std::size_t depth = 5) const {
        const auto book_it = books_.find(instrument_id);
        if (book_it == books_.end()) {
            throw std::runtime_error("Unknown instrument: " + instrument_id);
        }

        const BookSnapshot snap = book_it->second.snapshot(depth);
        std::cout << "\nBOOK " << instrument_id << '\n';
        std::cout << "  BIDS\n";
        for (const auto& level : snap.bids) {
            std::cout << "    " << format_price(level.price_ticks) << " x " << level.total_volume << '\n';
        }
        if (snap.bids.empty()) {
            std::cout << "    <empty>\n";
        }

        std::cout << "  ASKS\n";
        for (const auto& level : snap.asks) {
            std::cout << "    " << format_price(level.price_ticks) << " x " << level.total_volume << '\n';
        }
        if (snap.asks.empty()) {
            std::cout << "    <empty>\n";
        }
    }

    void print_help() const {
        std::cout
            << "Commands:\n"
            << "  HELP\n"
            << "  ADD <instrument>\n"
            << "  LIMIT <owner> <instrument> <bid|ask> <price> <volume>\n"
            << "  IOC <owner> <instrument> <bid|ask> <price> <volume>\n"
            << "  CANCEL <instrument> <order_id>\n"
            << "  BOOK <instrument> [depth]\n"
            << "  QUIT\n";
    }

private:
    std::unordered_map<std::string, OrderBook> books_;
    std::vector<Trade> trades_;
    std::uint64_t next_order_id_ = 1;
    std::uint64_t next_trade_id_ = 1;
    std::uint64_t next_sequence_ = 1;

    OrderBook& get_book(const std::string& instrument_id) {
        auto it = books_.find(instrument_id);
        if (it == books_.end()) {
            throw std::runtime_error("Unknown instrument: " + instrument_id);
        }
        return it->second;
    }

    void print_trade(const Trade& trade) const {
        std::cout
            << "TRADE "
            << trade.trade_id << ' '
            << trade.instrument << ' '
            << format_price(trade.price_ticks) << ' '
            << trade.volume << ' '
            << trade.buyer << ' '
            << trade.seller << '\n';
    }
};

}  // namespace optibook_like

int main() {
    using namespace optibook_like;

    Exchange exchange;
    exchange.add_instrument("OB5X_ETF");
    exchange.add_instrument("OB5X_202609_F");
    exchange.add_instrument("AMZN");

    std::cout << "Optibook-like exchange demo\n";
    exchange.print_help();

    std::string line;
    while (std::cout << "\nexchange> " && std::getline(std::cin, line)) {
        if (line.empty()) {
            continue;
        }

        std::istringstream input(line);
        std::string cmd;
        input >> cmd;

        try {
            if (cmd == "HELP") {
                exchange.print_help();
            } else if (cmd == "ADD") {
                std::string instrument;
                input >> instrument;
                exchange.add_instrument(instrument);
                std::cout << "Added instrument " << instrument << '\n';
            } else if (cmd == "LIMIT" || cmd == "IOC") {
                std::string owner;
                std::string instrument;
                std::string side_raw;
                double price = 0.0;
                int volume = 0;

                input >> owner >> instrument >> side_raw >> price >> volume;
                if (!input) {
                    throw std::runtime_error("Bad order syntax");
                }

                const OrderType type = cmd == "LIMIT" ? OrderType::Limit : OrderType::IOC;
                const Side side = parse_side(side_raw);
                const auto order_id = exchange.submit_order(owner, instrument, side, price, volume, type);
                std::cout << "Accepted order " << order_id << '\n';
            } else if (cmd == "CANCEL") {
                std::string instrument;
                std::uint64_t order_id = 0;
                input >> instrument >> order_id;
                if (!input) {
                    throw std::runtime_error("Bad cancel syntax");
                }

                const bool canceled = exchange.cancel_order(instrument, order_id);
                std::cout << (canceled ? "Canceled" : "Order not found") << '\n';
            } else if (cmd == "BOOK") {
                std::string instrument;
                std::size_t depth = 5;
                input >> instrument;
                if (input >> depth) {
                    exchange.print_book(instrument, depth);
                } else {
                    exchange.print_book(instrument);
                }
            } else if (cmd == "QUIT" || cmd == "EXIT") {
                break;
            } else {
                throw std::runtime_error("Unknown command: " + cmd);
            }
        } catch (const std::exception& ex) {
            std::cerr << "ERROR: " << ex.what() << '\n';
        }
    }

    return 0;
}
