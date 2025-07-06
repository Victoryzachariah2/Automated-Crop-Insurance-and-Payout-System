# Automated Crop Insurance & Payout System (ACIPS)

A decentralized crop insurance system built on Stacks blockchain that provides automatic payouts to farmers based on weather conditions.

## Features

- Purchase crop insurance with STX
- Automated payouts based on weather data
- Oracle-powered weather data feeds
- Transparent premium and payout tracking

## Contract Functions

### For Farmers

- `purchase-insurance`: Purchase insurance coverage for a specific region
- `cancel-insurance`: Cancel existing insurance coverage
- `claim-payout`: Request payout when eligible
- `get-farmer-info`: View insurance status and details

### For Oracle/Admin

- `update-weather-data`: Update weather data for regions
- `change-oracle`: Update oracle address (admin only)
- `get-weather-info`: View weather data for a region
- `get-contract-info`: View contract statistics

## Usage

1. Deploy contract using Clarinet
2. Purchase insurance by calling `purchase-insurance` with 1M STX
3. Oracle updates weather data periodically
4. Farmers can claim payouts when rainfall is below threshold

## Testing

```bash
clarinet test
```

## Deployment

```bash
clarinet deploy
```
```
